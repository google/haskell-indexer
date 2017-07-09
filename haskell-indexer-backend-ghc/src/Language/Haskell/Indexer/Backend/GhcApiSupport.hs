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

{-# LANGUAGE RecordWildCards #-}
-- | Deals with the gory details of starting up GHC and analysing a set of
-- targets.
module Language.Haskell.Indexer.Backend.GhcApiSupport
    ( withTypechecked
    , GhcArgs(..)
    , GhcEnv(..)
    ) where

-- GHC imports.
-- Note: use isHaskellishTarget in newer GHCs.
import DriverPhases (isHaskellSrcFilename)
import DynFlags
import GHC
import Outputable
import Util (looksLikeModuleName)

import Control.Concurrent.MVar (MVar, withMVar)
import Control.Monad ((>=>), forM_, void, when)
import Control.Monad.IO.Class
import qualified Data.List as L

import GHC.Paths (libdir)
import System.FilePath ((</>))
import System.Posix.Signals (installHandler, sigINT, Handler(Default))
import System.IO (hPutStrLn, stderr)

import Language.Haskell.Indexer.Translate (XRef(..))
import Language.Haskell.Indexer.Backend.GhcArgs
import Language.Haskell.Indexer.Backend.GhcEnv (GhcEnv(..))
import Language.Haskell.Indexer.Backend.Ghc (analyseTypechecked)
import Language.Haskell.Indexer.Backend.AnalysisOptions (AnalysisOptions)

printErr :: MonadIO m => String -> m ()
printErr = liftIO . hPutStrLn stderr

-- | Must be called serialized - due to some global linker state GHC API
-- can't process multiple compilations concurrently (see
-- https://mail.haskell.org/pipermail/ghc-devs/2014-January/003874.html).
withTypechecked
  :: MVar () -> GhcArgs -> AnalysisOptions -> (XRef -> IO ()) -> IO ()
withTypechecked globalLock GhcArgs{..} aopt action
        = withMVar globalLock . const . errHandling $ do
    -- TODO(robinpalotai): logging
    printErr "Running GHC"
    -- Make sure we don't use any tools except cpp.
    let modifyFlags = maybe id setPgmP (overridePgmP gaToolOverride)
                    . dontGenerateCode
                    . verbose 0
    gtm <- runGhc (Just $ gaLibdirPrefix </> libdir) $ do
        -- see GHC trac #4162
        liftIO . void $ installHandler sigINT Default Nothing
        dflags0 <- getSessionDynFlags
        (parsedFlags, unused, errors) <- parseDynamicFlags dflags0
                                            (map noLoc gaArgs)
        printErr $ "Yet unused flags (target candidates): "
            ++ L.intercalate "," (map unLoc unused)
        printErr $ "Flag errors: " ++ L.intercalate "," (map unLoc errors)
        let dflags = modifyFlags parsedFlags
        _  <- setSessionDynFlags dflags
        forM_ (filter isHaskellSource . map unLoc $ unused) $ \path ->
            guessTarget path Nothing >>= addTarget
        ts <- getTargets
        printErr $ "Targets:" ++ L.intercalate "," (map (showPpr dflags) ts)
        graph <- depanal [] False
        when (needsTemplateHaskell graph) $
            printErr "Some modules need TH - note: we don't gen code for now."
        printErr "load"
        void $ load LoadAllTargets
        usedDflags <- getSessionDynFlags
        let env = GhcEnv (showSDoc usedDflags . ppr)
                         (showSDocForUser usedDflags neverQualify . ppr)
        mapM (parseModule >=> typecheckModule >=> (analyseTypechecked env aopt)) graph
    mapM_ action gtm
 where
    errHandling = defaultErrorHandler defaultFatalMessager defaultFlushOut
    isHaskellSource src = isHaskellSrcFilename src || looksLikeModuleName src
    --
    dontGenerateCode dflags = dflags
        { hscTarget = HscNothing, ghcLink = NoLink }
    -- | Note: v=3 is the regular command-line "-v".
    verbose v dflags = dflags { verbosity = v }
    --
    setPgmP f = alterSettings (\s -> s { sPgm_P = (f, (snd . sPgm_P) s)})
    --
    alterSettings :: (Settings -> Settings) -> DynFlags -> DynFlags
    alterSettings f dflags = dflags { settings = f (settings dflags) }
