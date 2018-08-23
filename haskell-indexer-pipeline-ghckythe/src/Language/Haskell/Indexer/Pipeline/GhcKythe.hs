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

{-# LANGUAGE BangPatterns #-}
-- | Basic building block for streamed indexing using GHC backend and Kythe
-- frontend.
module Language.Haskell.Indexer.Pipeline.GhcKythe
    ( ghcToKythe
    ) where

import Control.Concurrent.MVar (MVar)
import Control.Monad.Identity (runIdentity)
import Control.Monad.Morph (lift, hoist)
import qualified Data.ByteString as B
import Data.Conduit (($$), (=$=), await, awaitForever, yield, Conduit, Sink, transPipe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T

import Language.Haskell.Indexer.Translate
import Language.Haskell.Indexer.Backend.AnalysisOptions (AnalysisOptions)
import Language.Haskell.Indexer.Backend.GhcArgs (GhcArgs)
import Language.Haskell.Indexer.Backend.GhcApiSupport (withTypechecked)
import Language.Haskell.Indexer.Frontend.Kythe (toKythe)

import qualified Language.Kythe.Schema.Raw as Raw

ghcToKythe
    :: MVar () -> GhcArgs -> AnalysisOptions
    -> Raw.VName
    -> ([Raw.Entry] -> IO ())
    -> IO ()
ghcToKythe globalLock ghcArgs analysisOptions baseVName sink =
    withTypechecked globalLock ghcArgs analysisOptions collect
  where
    collect xref = do
        sourceText <- lenientDecodeUtf8 . T.unpack
                    . unSourcePath . analysedTempPath . xrefFile
                    $ xref
        -- Note: since this Conduit pipeline is pretty context-dependent, there
        -- is low chance of leak due to accidental sharing (see
        -- https://www.well-typed.com/blog/2016/09/sharing-conduit/).
        transPipe (return . runIdentity) (toKythe baseVName sourceText xref
            -- Batch an ad-hoc number of entries to be emitted together.
            =$= chunksOf 1000)
            $$ sinkChunks
    --
    sinkChunks :: Sink [Raw.Entry] IO ()
    sinkChunks = awaitForever (lift . sink)

-- | Haskell sources are de-facto UTF-8, but GHC ignores bad bytes in comments.
-- See http://stackoverflow.com/questions/6797902/haskell-source-encoding.
--
-- Here we assume that the processed sources already compile with GHC, so
-- lenient decoding suffices.
--
-- Note: once we start extracting comments with Haddock API, the in-comment
-- offsets might be off if there are incorrect sequences. But also depends on
-- how Haddock behaves, and pretty edge case too.
lenientDecodeUtf8 :: FilePath -> IO T.Text
lenientDecodeUtf8 = fmap (T.decodeUtf8With T.lenientDecode) . B.readFile

-- | Group a stream into chunks of a given size. The last chunk may contain
-- fewer than n elements.
--
-- Note: present in upstream Conduit 1.2.9.
chunksOf :: Monad m => Int -> Conduit a m [a]
chunksOf n = start
  where
    start = await >>= maybe (return ()) (\x -> loop n (x:))
    loop !count rest = await >>= maybe (yield (rest [])) go
      where
        go y | count > 1 = loop (count - 1) (rest . (y:))
             | otherwise = yield (rest []) >> loop n (y:)
