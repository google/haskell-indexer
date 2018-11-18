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
import Language.Haskell.Indexer.Translate
import Language.Haskell.Indexer.Args

import System.IO
import System.FilePath
import System.Directory

plugin :: Plugin
plugin = defaultPlugin
          { typeCheckResultAction = install }

mkPath :: FilePath -> Module -> FilePath
mkPath fp m
  = fp </> (moduleNameString (moduleName m) ++ (show (moduleUnitId m)))

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

      let action aoes =
            unGhc (analyseTypechecked' env aoes ms
                    (getRenamedStuff tc_gbl)
                    (tcg_binds tc_gbl)
                    (\_ -> ModuleTick (PkgModule "abc" "def") Nothing))
                    session
      liftIO $ kythePlugin h action flags
      return tc_gbl

