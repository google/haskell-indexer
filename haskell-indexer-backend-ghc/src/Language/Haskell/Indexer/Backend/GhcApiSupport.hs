-- Copyright 2017 Google Inc.
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
-- | Deals with the gory details of starting up GHC and analysing a set of
-- targets.
module Language.Haskell.Indexer.Backend.GhcApiSupport
    ( withTypechecked
    , GhcArgs(..)
    , GhcEnv(..)
    ) where

-- GHC imports.
import CmdLineParser (Warn(warnMsg))
import DriverPhases (isHaskellishTarget, Phase(StopLn))
import DriverPipeline (compileFile)
import DynFlags
import DynamicLoading (initializePlugins)
import GHC
import qualified Linker
import Outputable
import qualified Pretty

import Control.Arrow ((&&&))
import Control.Concurrent.MVar (MVar, withMVar)
import Control.Monad ((>=>), forM_, unless, void)
import Control.Monad.IO.Class
import qualified Data.List as L
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T

import GHC.Paths (libdir)
import Network.URI (escapeURIString, isUnescapedInURIComponent)
import System.FilePath ((</>))
import System.Posix.Signals (installHandler, sigINT, Handler(Default))
import System.IO (hPutStrLn, stderr)

import Language.Haskell.Indexer.Translate
    ( DocUriDecl(..)
    , PkgModule(..)
    , Tick(..)
    , TickReference(..)
    , XRef(..)
    )
import Language.Haskell.Indexer.Backend.GhcArgs
import Language.Haskell.Indexer.Backend.GhcEnv (GhcEnv(..))
import Language.Haskell.Indexer.Backend.Ghc (analyseTypechecked)
import Language.Haskell.Indexer.Backend.AnalysisOptions (AnalysisOptions)

printErr :: MonadIO m => String -> m ()
printErr = liftIO . hPutStrLn stderr

-- This should probably be moved to GHC's Outputable module.
-- Combined form of 'showSDocOneLine' and 'showSDocForUser'.
showSDocForUserOneLine :: DynFlags -> PrintUnqualified -> SDoc -> String
showSDocForUserOneLine dflags unqual doc =
  let s = Pretty.style { Pretty.mode = Pretty.OneLineMode
                       , Pretty.lineLength = pprCols dflags
                       }
   in Pretty.renderStyle s $
        runSDoc doc (initSDocContext dflags (mkUserStyle dflags unqual AllTheWay))

-- | Must be called serialized - due to some global linker state GHC API
-- can't process multiple compilations concurrently (see
-- https://mail.haskell.org/pipermail/ghc-devs/2014-January/003874.html).
withTypechecked
    :: MVar () -> GhcArgs -> AnalysisOptions -> (XRef -> IO ()) -> IO ()
withTypechecked globalLock GhcArgs{..} analysisOpts xrefSink
        = withMVar globalLock . const $ do
    -- TODO(robinpalotai): logging
    printErr "Running GHC"
    xrefGraph <- runGhc (Just $ gaLibdirPrefix </> libdir) $ do
        -- see GHC trac #4162
        liftIO . void $ installHandler sigINT Default Nothing
        -- Keeping the current dflags in mutable state, to avoid accidentally
        -- using a non-current version amidst frequent mutations.
        let ghcDflagOp :: (DynFlags -> Ghc a) -> Ghc a
            ghcDflagOp f = getSessionDynFlags >>= f
            pureDflagOp :: (DynFlags -> a) -> Ghc a
            pureDflagOp f = ghcDflagOp (return . f)
            -- | Warning: don't call this (or setSessionDynFlags) before
            -- parsing the actual arguments! It seems the first call to
            -- setSessionDynFlags caches the package databases, and if you miss
            -- passing the custom dbs from the command line, later invocations
            -- won't fix that and GHC won't find those packages.
            modifyDflags :: (DynFlags -> DynFlags) -> Ghc ()
            modifyDflags f =
                fmap f GHC.getSessionDynFlags >>= void . GHC.setSessionDynFlags
        -- The obscure preparation that follows is explained in
        -- https://gist.github.com/robinp/49c68c6f69f6aabfddee3cba42b4964f,
        -- further referenced below as 'the gist'.
        printErr $ "GHC arguments: " ++ show gaArgs
        (unusedArgs, (defaultHscTarget, defaultLink)) <- do
            (newDflags, unused, errors) <- ghcDflagOp (flip parseDynamicFlags
                                                        (map noLoc gaArgs))
            modifyDflags (verbose 0 . const newDflags)
            defaultTargetAndLink <- pureDflagOp (hscTarget &&& ghcLink)
            unless (null errors) $
                -- TODO(robinpalotai): error out.
                printErr $ "Flag errors: "
                              ++ L.intercalate ", " (map getWarnMsg errors)
            return (unused, defaultTargetAndLink)
        ghcDflagOp (\d -> printErr
                      $ show ("Codegen state after flag parse" :: String,
                              hscTarget d, ghcLink d))
        -- After dynamic flag parsing, what remains: Haskell and non-Haskell
        -- sources, object files/shared libs, RTS options. Also --make and such,
        -- which is normally stripped by GHC's Main.
        let (rtsArgs, nonRtsArgs) = partitionRtsArgs . map unLoc $ unusedArgs
            (droppedArgs, inputFiles) = L.partition ("--" `L.isPrefixOf`)
                                                    nonRtsArgs
        -- Separate so we can compile non-Haskell files if needed.
        -- TODO(robinpalotai): from the resulting non_hs_srcs, split the object
        --     files and add them to ldOptions directly. See partition_args in
        --     GHC's ghc/Main.hs.
        let (hs_srcs, non_hs_srcs) = L.partition isHaskellishTarget
                                   . map (,Nothing)
                                   $ inputFiles
            report = [ ("RTS args (unused for indexing)", rtsArgs)
                     , ("Dropped args", droppedArgs)
                     , ("Haskell targets", map fst hs_srcs)
                     , ("Non-haskell sources", map fst non_hs_srcs)
                     ]
        forM_ report $ \(description, as) ->
            printErr $ description ++ ": " ++ L.intercalate ", " as
        -- Prepare Haskell targets to find out which compilation/link level is
        -- needed.
        hsTargets <- mapM (uncurry GHC.guessTarget) hs_srcs
        GHC.setTargets hsTargets
        -- Note: depanal caches current DynFlags into module dflags.
        graphPreliminary <- depanal [] False
        if not (needsTemplateHaskellOrQQ graphPreliminary)
        then modifyDflags dontGenerateCode
        else do
            -- Actually very few TH usage needs code generation, but it's
            -- hard to tell upfront if that's the case: see the gist or
            -- the tests for an example splice pattern that needs it.
            --
            -- If we could tell with confidence that compile-time code
            -- execution would not happen, could skip this part.
            --
            -- Alternatively, the graph returned by depanal could be
            -- processed fine-grained, and modules downstream of TH-using
            -- ones surely don't need code generation.
            let anyOptimization = any (hasOpt . ms_hspp_opts)
                                      (mgModSummaries graphPreliminary)
                  where hasOpt = (>0) . optLevel
            -- Foreign exports are not compatible with HscInterpreted,
            -- giving a compile panic.
            -- TODO(robinpalotai): actually scan for this.
            let hasForeignExport = False
            -- Optimization is not compatible with HscInterpreted, which
            -- results in a warning. But if -Werror is also on, that would
            -- result in an error, and indexing would fail. So we rather
            -- switch to machine code generation.
            --
            -- Question: why don't we just switch off -Werror during
            -- indexing (if present)? We might want to index the warnings as
            -- diagnostic messages, so better keep them.
            modifyDflags $ \d ->
                if anyOptimization || hasForeignExport
                -- The default is machine code generation (though some GHC args
                -- can change this, but we hope they change in a reasonable
                -- direction).
                then d { hscTarget = defaultHscTarget, ghcLink = defaultLink }
                else d { hscTarget = HscInterpreted, ghcLink = LinkInMemory }
            -- Compile non-Haskell, for example C inputs, since TH might use
            -- them.
            hsc_env <- GHC.getSession
            o_files <- liftIO $ mapM (compileFile hsc_env StopLn) non_hs_srcs
            modifyDflags $ \d ->
                    d { ldInputs = map (FileOption "") o_files ++ ldInputs d }
            magicLink
        -- Proceed with compiling Haskell.
        ghcDflagOp $ \d -> printErr
                        $ show ("Codegen state after setting up TH" :: String,
                                hscTarget d, ghcLink d)
        graph <- depanal [] False  -- DynFlags may have changed, so call again.
        ghcDflagOp $ \d -> printErr $ "Loading Haskell targets:"
                               ++ L.intercalate "," (map (showPpr d) hsTargets)
        void $ GHC.load LoadAllTargets
        printErr "Loaded Haskell targets"
        usedDflags <- getSessionDynFlags
        let env = GhcEnv (showSDocOneLine usedDflags . ppr)
                         (showSDocForUserOneLine usedDflags neverQualify . ppr)
            extractXref = analyseTypechecked env analysisOpts
        mapM (loadModulePlugins >=> parseModule >=> typecheckModule >=> extractXref)
                (mgModSummaries graph)
    let xrefGraph' = attachDocUriDecls xrefGraph
    mapM_ xrefSink xrefGraph'
 where
    getWarnMsg :: Warn -> String
    getWarnMsg = unLoc . warnMsg
    -- | RTS args would tune performance of the compilation. But we can't set
    -- them per-compilation from 'GhcApiSupport', so drop them.
    partitionRtsArgs :: [String] -> ([String], [String])
    partitionRtsArgs = go False [] []
      where go inRts rts nonRts args = case args of
                [] -> (rts, nonRts)
                (a:as) -> case a of
                    "+RTS" -> go True rts nonRts as
                    "-RTS" -> go False rts nonRts as
                    _      -> if inRts then go True (a:rts) nonRts as
                                       else go False rts (a:nonRts) as
    --
    dontGenerateCode dflags = dflags
        { hscTarget = HscNothing, ghcLink = NoLink }
    -- | Note: v=3 is the regular command-line "-v".
    verbose v dflags = dflags { verbosity = v }
    -- | Tries to replicate loading logic found in 'reallyInitDynLinker', which
    -- is only called once per GHC(API) process, so not suitable for repeated
    -- calls.
    --
    -- Will fail if we try to load an FFI object that exports a name we already
    -- saw before. This is a realistic risk when indexing many packages, see the
    -- gist for discussion.
    magicLink :: Ghc ()
    magicLink = do
        printErr "MagicLink happens."
        state <- GHC.getSession
        liftIO $ do
            Linker.initDynLinker state
            -- As described in the gist, this doesn't unload everything, but at
            -- least we try.
            Linker.unload state []
            Linker.linkCmdLineLibs state
            -- TODO(robinpalotai): link packages? See 'reallyInitDynLinker'.
            -- This might be needed if TH executes code from other package? Or
            -- only if that code needs FFI?

-- | Doc URI decls are for things that don't have source code available; most
-- likely those from core packages. Hence their decls may be duplicated coming
-- from multiple analyses. Deduplicate them, and put them in the 'xrefDocDecls'
-- field of the first 'XRef'.
attachDocUriDecls :: [XRef] -> [XRef]
attachDocUriDecls graph =
    let tickRefs = concatMap xrefCrossRefs graph
        docDecls = generateDocUriDecls tickRefs
     in case graph of
            [] -> []
            x : xs -> x {xrefDocDecls = docDecls} : xs

generateDocUriDecls :: [TickReference] -> [DocUriDecl]
generateDocUriDecls rs =
  let declMap =
        foldr
          ( \r decls ->
              let targetTick = refTargetTick r
               in case docUri targetTick of
                    Nothing -> decls
                    Just uri -> addDecl uri targetTick decls
          )
          M.empty
          rs
   in M.elems declMap
  where
    docUri tick = case tickSpan tick of
      -- An empty name span probably means something from core packages.
      -- Associate with the hackage document.
      Nothing -> Just $ hackageSrcUrl tick
      Just _ -> Nothing
    addDecl uri targetTick decls
      | M.member uri decls = decls
      | otherwise =
        let decl = DocUriDecl {ddeclTick = targetTick, ddeclDocUri = uri}
         in M.insert uri decl decls

hackageSrcUrl :: Tick -> Text
hackageSrcUrl tick =
  let pm = tickPkgModule tick
      pkg = getPackageWithVersion pm
      modName = getModule pm
      name = tickThing tick
   in T.concat
        [ "https://hackage.haskell.org/package/"
        , escape pkg
        , "/docs/src/"
        , escape modName
        , ".html#"
        , escape name
        ]
  where
    escape :: Text -> Text
    escape = T.pack . escapeURIString isUnescapedInURIComponent . T.unpack

-- | Each module needs its plugins loaded explicitly.
loadModulePlugins :: ModSummary -> Ghc ModSummary
loadModulePlugins modsum = do
    hsc_env <- getSession
    dynflags' <- liftIO (initializePlugins hsc_env (ms_hspp_opts modsum))
    return $ modsum { ms_hspp_opts = dynflags' }
