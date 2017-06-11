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
    , analyseAndMerge
    , mergeRefs
    -- * Combined helpers.
    , assertXRefsFrom
    -- * Reexports from TranslateAssert.
    , module Language.Haskell.Indexer.Backend.Ghc.Test.TranslateAssert
    ) where

import Control.Concurrent.MVar (MVar, newMVar)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT, asks, runReaderT)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Foldable as Foldable
import qualified Data.IORef as IO
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import System.FilePath ((</>))
import System.IO.Unsafe (unsafePerformIO)

import Language.Haskell.Indexer.Backend.GhcArgs
import Language.Haskell.Indexer.Backend.Ghc (analyseTypechecked)
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

-- | Convenience for asserting analysis of a testdata-relative file.
assertXRefsFrom :: FilePath -> ReaderT XRef IO () -> ReaderT TestEnv IO ()
assertXRefsFrom f asserts = do
    xref <- analyseAndMerge f
    liftIO (runReaderT asserts xref)

-- | Analyses a test file relative to the 'data' directory.
analyse :: FilePath -> ReaderT TestEnv IO (NonEmpty XRef)
analyse dataRelative = do
    fullPath <- (</> dataRelative) <$> asks testDataDirectory
    analyiseFullPath fullPath

-- | Convenience for disregarding the source file of reference data.
analyseAndMerge :: FilePath -> ReaderT TestEnv IO XRef
analyseAndMerge = fmap mergeRefs . analyse

-- | Merges with terrible list concat performance, keeps an arbitrary file
-- name.
mergeRefs :: NonEmpty XRef -> XRef
mergeRefs = Foldable.foldr1 merge
  where
    merge a b = XRef
        { xrefFile = xrefFile a
        , xrefModule = xrefModule a
        , xrefDecls = xrefDecls a ++ xrefDecls b
        , xrefCrossRefs = xrefCrossRefs a ++ xrefCrossRefs b
        , xrefRelations = xrefRelations a ++ xrefRelations b
        , xrefImports = xrefImports a ++ xrefImports b
        }

analyiseFullPath :: FilePath -> ReaderT TestEnv IO (NonEmpty XRef)
analyiseFullPath f = do
    baseGhcArgs <- asks testDefaultGhcArgs
    let ghcArgs = baseGhcArgs { gaArgs = [f] }
    refs <- lift $ do
        res <- IO.newIORef []
        withTypechecked globalLock ghcArgs (saveAnalysedTo res)
        IO.readIORef res
    if null refs
        then error "Unexpected: withTypechecked didn't produce any result."
        else return $! NonEmpty.fromList refs
  where
    saveAnalysedTo xs ghcEnv tm =
        let opts = defaultAnalysisOptions { aoMainPkgFallback = "dummyPkg" }
            xref = analyseTypechecked ghcEnv opts tm  --
        in IO.modifyIORef' xs (xref:)
