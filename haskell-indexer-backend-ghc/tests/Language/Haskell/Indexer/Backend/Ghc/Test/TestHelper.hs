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

{- Helpers for testing extraction functionality.
 -
 - For now hardcoded for GHC backend. If multiple backends would be available,
 - should make it injectable.
 -}
{-# LANGUAGE OverloadedStrings #-}
module Language.Haskell.Indexer.Backend.Ghc.Test.TestHelper
    ( TestEnv(..)
    , analyse
    , mergeRefs
    -- * Combined helpers.
    , assertXRefsFrom
    , assertXRefsFromExtra
    -- * Reexports from TranslateAssert.
    , module Language.Haskell.Indexer.Backend.Ghc.Test.TranslateAssert
    ) where

import Control.Concurrent.MVar (MVar, newMVar)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT, asks, runReaderT)
import Control.Monad.IO.Class (liftIO)
import Data.Either (lefts)
import qualified Data.Foldable as Foldable
import qualified Data.IORef as IO
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import qualified System.Directory as Dir
import System.FilePath ((</>))
import qualified System.FilePath as File
import System.IO.Temp (withTempDirectory)
import System.IO.Unsafe (unsafePerformIO)

import Language.Haskell.Indexer.Backend.GhcArgs
import Language.Haskell.Indexer.Backend.AnalysisOptions
import Language.Haskell.Indexer.Backend.GhcApiSupport (withTypechecked)
import Language.Haskell.Indexer.Backend.Ghc.Test.TranslateAssert  -- to reexport

-- | A sin against humanity in the name of easier testing.
-- This is the evil but de-facto global variable pattern.
globalLock :: MVar ()
globalLock = unsafePerformIO (newMVar ())
{-# NOINLINE globalLock #-}

data TestEnv = TestEnv
    { testDataDirectory :: FilePath
      -- ^ Path up to the data directory containing subdirs per test.
    , testDefaultGhcArgs :: GhcArgs
      -- ^ Defaults populated with locations of various tools.
    }

-- | Like 'assertXRefsFromExtra' but without extra files to copy.
assertXRefsFrom :: [String] -> ReaderT XRef IO () -> ReaderT TestEnv IO ()
assertXRefsFrom = assertXRefsFromExtra []

-- | Convenience for asserting analysis of testdata-relative files and other
-- arguments. The 'extraFiles' get added to the input directory, but don't get
-- passed on the GHC command line.
assertXRefsFromExtra
    :: [FilePath] -> [String] -> ReaderT XRef IO ()
    -> ReaderT TestEnv IO ()
assertXRefsFromExtra extraFiles args asserts = do
    xref <- fmap mergeRefs (analyse extraFiles args)
    liftIO (runReaderT asserts xref)

-- | Analyses invocation with given args, relative to the 'data' directory.
analyse :: [String] -> [String] -> ReaderT TestEnv IO (NonEmpty XRef)
analyse extraFiles relativeArgs = do
    dataDir <- asks testDataDirectory
    let convertToFullPath = mapM (makeFullPath dataDir)
    args <- liftIO $ convertToFullPath relativeArgs
    extraFullPaths <- fmap lefts . liftIO $ convertToFullPath extraFiles
    analyiseFullPath extraFullPaths args
  where
    makeFullPath :: FilePath -> String -> IO Arg
    makeFullPath baseDir arg = do
        let candidatePath = baseDir </> arg
        exists <- Dir.doesFileExist candidatePath
        return $ if not exists then Right arg  -- Not a file arugment.
                 else Left candidatePath

-- | We can either pass (here absolute) filenames or free-form strings as GHC
-- options.
type Arg = Either FilePath String

-- | Merges with terrible list concat performance, keeps an arbitrary file
-- name.
mergeRefs :: NonEmpty XRef -> XRef
mergeRefs = Foldable.foldr1 merge
  where
    merge a b = XRef
        { xrefFile = xrefFile a
        , xrefModule = xrefModule a
        , xrefDecls = xrefDecls a ++ xrefDecls b
        , xrefDocDecls = xrefDocDecls a ++ xrefDocDecls b
        , xrefCrossRefs = xrefCrossRefs a ++ xrefCrossRefs b
        , xrefRelations = xrefRelations a ++ xrefRelations b
        , xrefImports = xrefImports a ++ xrefImports b
        }

analyiseFullPath :: [FilePath] -> [Arg] -> ReaderT TestEnv IO (NonEmpty XRef)
analyiseFullPath extraPaths args = do
    tmpTop <- liftIO Dir.getTemporaryDirectory
    -- Put resources of each test into separate tmp dir to prevent crosstalk.
    withTempDirectory tmpTop "ghctest." $ \tmp -> do
        mirrors <- liftIO $ do
            mapM_ (makeMirror tmp) extraPaths
            mapM (either (makeMirror tmp) return) args
        baseGhcArgs <- asks testDefaultGhcArgs
        let ghcArgs = baseGhcArgs { gaArgs = gaArgs baseGhcArgs ++ mirrors }
        refs <- lift $ do
            res <- IO.newIORef []
            let opts = defaultAnalysisOptions { aoMainPkgFallback = "dummyPkg" }
            withTypechecked globalLock ghcArgs opts (saveAnalysedTo res)
            IO.readIORef res
        if null refs
            then error "Unexpected: withTypechecked didn't produce any result."
            else return $! NonEmpty.fromList refs
  where
    -- | Needed due to https://ghc.haskell.org/trac/ghc/ticket/14025 to avoid
    -- putting output in the input tree (which won't work for a read-only input
    -- tree).
    makeMirror tmp f = do
        -- If 'f' has an absolute path, drop the leading slashes so it can
        -- be appended to the tmp root (since "foo" </> "/bar" == "/bar").
        let newDir = tmp </> dropWhile (== '/') (File.takeDirectory f)
            newFile = newDir </> File.takeFileName f
        Dir.createDirectoryIfMissing True newDir
        Dir.copyFile f newFile
        return newFile
    saveAnalysedTo xs xref = IO.modifyIORef' xs (xref:)
