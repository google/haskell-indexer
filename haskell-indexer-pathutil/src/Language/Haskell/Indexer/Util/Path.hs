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

module Language.Haskell.Indexer.Util.Path
    ( asTextPath
    , stripTmpPrefix
    ) where

import Data.List (isPrefixOf)
import qualified Data.Text as T
import System.FilePath (splitPath, joinPath)

-- | Removes the workdir prefix from likely temporary paths.
-- Useful for 'aoFilePathTransform' in 'AnalysisOptions'.
stripTmpPrefix :: FilePath -> FilePath
stripTmpPrefix path = case partsAfterTmp path of
    Just (rand:ps) | isValidRandomDir rand -> joinPath ps
    _ -> path
  where
    -- | Finds the longest suffix of the path that begins with "tmp/".
    partsAfterTmp :: FilePath -> Maybe [String]
    partsAfterTmp = go . splitPath
      where go [] = Nothing
            go ("tmp/":rest) = Just rest
            go (_:rest) = go rest
    -- | True if the directory matches some known random-generation pattern.
    isValidRandomDir :: String -> Bool
    isValidRandomDir dir = all (`elem` '/':['0'..'9']) dir
        || "tempfile" `isPrefixOf` dir

-- | Wraps a FilePath operation as a Text operation.
asTextPath :: (FilePath -> FilePath) -> T.Text -> T.Text
asTextPath f = T.pack . f . T.unpack
