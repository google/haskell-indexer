{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Haskell.Indexer.Plugin where

import GhcPlugins hiding (($$))
import TcRnTypes

import TcRnMonad
import TcRnDriver
import GhcMonad
import Data.IORef

import Language.Haskell.Indexer.Backend.Ghc
import Language.Haskell.Indexer.Backend.GhcEnv
import Language.Haskell.Indexer.Args

import System.IO
import System.FilePath
import System.Directory

-- Defining a <https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/extending_ghc.html?highlight=plugin#compiler-plugins source plugin>
-- which can be dynamically loaded when running GHC with the `-plugin` flag.
plugin :: Plugin
plugin = defaultPlugin
          { renamedResultAction   = keepRenamedSource
          , typeCheckResultAction = install }

-- Makes a unique path for each module so that we don't clobber each the
-- entries when compiling different modules. The name of the file isn't
-- important as long as it is different for each one.
mkPath :: FilePath -> Module -> FilePath
mkPath fp m
  = fp </> (moduleNameString (moduleName m) ++ (show (moduleUnitId m)))
       <.> "entries"

-- This function is invoked once at the end of compiling every module.
-- It runs the indexer and then outputs the entries to a directory.
install :: [CommandLineOption] -> ModSummary -> TcGblEnv -> TcM TcGblEnv
install opts ms tc_gbl = do
      dflags <- getDynFlags
      hsc_env <- env_top <$> getEnv
      session <- Session <$> liftIO (newIORef hsc_env)
      flags <- liftIO (wrapperParser opts)
      let outdir = case flagOutput flags of
                       Nothing -> "haskell-indexer"
                       Just o -> o
          outpath = mkPath outdir (ms_mod ms)
      liftIO $ createDirectoryIfMissing False outdir
      h <- liftIO (openFile outpath WriteMode)
      let env = GhcEnv (showSDoc dflags . ppr)
                       (showSDocForUser dflags neverQualify . ppr)
          action aoes =
            unGhc (analyseTypechecked' env aoes ms
                    (getRenamedStuff tc_gbl)
                    (tcg_binds tc_gbl)
                    -- TODO: Fill in the Nothing with the correct span
                    (ms_mod ms, Nothing))
                    session
      liftIO $ kythePlugin h action flags
      return tc_gbl

