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
#if __GLASGOW_HASKELL__ >= 800
import DriverPhases (isHaskellishTarget, Phase(StopLn))
#else
import DriverPhases (isHaskellSrcFilename, Phase(StopLn))
import Util (looksLikeModuleName)
#endif
import DriverPipeline (compileFile)
import DynFlags
import GHC
import qualified Linker
import Outputable

import Control.Arrow ((&&&))
import Control.Concurrent.MVar (MVar, withMVar)
import Control.Monad ((>=>), forM_, unless, void, when)
import Control.Monad.IO.Class
import qualified Data.IORef as I
import qualified Data.List as L

import GHC.Paths (libdir)
import System.FilePath ((</>))
import qualified System.Directory as Dir
import System.Posix.Signals (installHandler, sigINT, Handler(Default))
import System.IO (hPutStrLn, stderr)

import Language.Haskell.Indexer.Translate (XRef(..))
import Language.Haskell.Indexer.Backend.GhcArgs
import Language.Haskell.Indexer.Backend.GhcEnv (GhcEnv(..))
import Language.Haskell.Indexer.Backend.Ghc (analyseTypechecked)
import Language.Haskell.Indexer.Backend.AnalysisOptions (AnalysisOptions)

import Language.Haskell.Indexer.Backend.Compat

#if __GLASGOW_HASKELL__ < 800
isHaskellishTarget :: (FilePath, Maybe Phase) -> Bool
isHaskellishTarget (src,_) =
    isHaskellSrcFilename src || looksLikeModuleName src
#endif

printErr :: MonadIO m => String -> m ()
printErr = liftIO . hPutStrLn stderr

-- | Must be called serialized - due to some global linker state GHC API
-- can't process multiple compilations concurrently (see
-- https://mail.haskell.org/pipermail/ghc-devs/2014-January/003874.html).
withTypechecked
    :: MVar () -> GhcArgs -> AnalysisOptions -> (XRef -> IO ()) -> IO ()
withTypechecked globalLock GhcArgs{..} analysisOpts xrefSink
        = withMVar globalLock . const . errHandling $ do
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
        ghcDflagOp (\d -> printErr $ show ("Codegen state after flag parse",
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
        ghcDflagOp $ \d -> printErr $ show ("Codegen state after setting up TH",
                               hscTarget d, ghcLink d)
        graph <- depanal [] False  -- DynFlags may have changed, so call again.
        ghcDflagOp $ \d -> printErr $ "Loading Haskell targets:"
                               ++ L.intercalate "," (map (showPpr d) hsTargets)
        void $ GHC.load LoadAllTargets
        printErr "Loaded Haskell targets"
        usedDflags <- getSessionDynFlags
        let env = GhcEnv (showSDoc usedDflags . ppr)
                         (showSDocForUser usedDflags neverQualify . ppr)
        let extractXref = analyseTypechecked env analysisOpts
        mapM (parseModule >=> typecheckModule >=> extractXref) (mgModSummaries graph)
    mapM_ xrefSink xrefGraph
 where
    errHandling = defaultErrorHandler defaultFatalMessager defaultFlushOut
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
    --
    alterSettings :: (Settings -> Settings) -> DynFlags -> DynFlags
    alterSettings f dflags = dflags { settings = f (settings dflags) }
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
#if __GLASGOW_HASKELL__ >= 800
        state <- GHC.getSession
#else
        state <- getSessionDynFlags
#endif
        liftIO $ do
            Linker.initDynLinker state
            -- As described in the gist, this doesn't unload everything, but at
            -- least we try.
            Linker.unload state []
            Linker.linkCmdLineLibs state
            -- TODO(robinpalotai): link packages? See 'reallyInitDynLinker'.
            -- This might be needed if TH executes code from other package? Or
            -- only if that code needs FFI?
