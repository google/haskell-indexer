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

{-# LANGUAGE BangPatterns, OverloadedStrings, RecordWildCards #-}
-- | Helpers for calculating char/byte offset from line and column number.
-- All line and column offsets in this module are zero-based.
--
-- Note: currently only line/column to byte offset calculation is exposed, but
-- the 'OffsetTable' structure supports more, which can be added on demand.

module Data.Text.Offset
  ( OffsetTable
  -- * Creation
  , createOffsetTable
  -- * Querying
  , lineColToByteOffsetDetail
  , lineColToByteOffset
  , OffsetError(..)
  , OverLineKind(..)
  ) where

import Control.Monad (unless)
import Data.Foldable (foldl')
import Data.Char (ord)
import qualified Data.Text.Lazy as TL
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV

-- | Unicode code point count (aka number of Haskell Chars).
type CharOffset = Int
-- | Raw byte offset, according to UTF-8 encoding.
type ByteOffset = Int
-- | Offsets at the beginning of a line.
type LineOffsets = (CharOffset, ByteOffset)

-- | Number of bytes a Char takes when UTF-8 encoded.
type CharBytes = Int
-- | Offset of Char in the line (zero-based) and its encoded byte count (>1).
type MultibyteChar = (Int, CharBytes)
-- | Content char count, excluding the end-of-line separator.
type ContentCharCount = Int

type LineCharInfo = (ContentCharCount, UV.Vector MultibyteChar)

-- | Stores info for mapping line/column number (Char-based) to raw offset
-- (either Char or UTF-8 encoded bytes-based).
data OffsetTable = OffsetTable
    { lineOffsets :: !(UV.Vector LineOffsets)
    , lineCharInfo :: !(V.Vector LineCharInfo)
    } deriving (Eq, Show)

-- | Calculates the 'OffsetTable' for the given text. The table calculation is
-- not optimized (non-strict tuples are lying around), but lookups on the result
-- are performant (since the tuples are stored as unboxed vectors).
createOffsetTable :: TL.Text -> OffsetTable
createOffsetTable
    = adjust
    . foldl' addLineOffsets [((0, 0), undefined)]
    . TL.splitOn "\n"
  where
    addLineOffsets
        :: [(LineOffsets, LineCharInfo)]
        -> TL.Text
        -> [(LineOffsets, LineCharInfo)]
           -- ^ 'LineOffsets' of next line (if any), multibytes of current line.
           --   Accumulated in reversed line order.
    addLineOffsets ofs@(((cs,bs), _):_) s =
        let !charCount = fromIntegral (TL.length s)
            multis = multibytes s
            !byteCount = bytesUntilCharPosition charCount multis
            !newOffs = (cs + charCount + 1, bs + byteCount + 1)
                        -- +1 for newlines
        in (newOffs, (charCount, multis)):ofs
    addLineOffsets _ _ = error "addLineOffsets call with empty accumulator"
    -- | Drops last offset, and first (bogus) charinfo.
    adjust :: [(LineOffsets, LineCharInfo)] -> OffsetTable
    adjust xs =
        let (ofs, info) = unzip xs
        in OffsetTable
            { lineOffsets = UV.fromList . reverse . tail $ ofs
            , lineCharInfo = V.fromList . tail . reverse $ info
            }

data OffsetError
    = NoSuchLine
    | EmptyLine
    | OverLineEnd !ByteOffset !OverLineKind
      -- ^ Clients might want to recover from this case, see 'JustAtLineEnd'.
    deriving (Eq, Ord, Show)

data OverLineKind
    = JustAtLineEnd
      -- ^ Referenced position is just after the line content. Position can be
      --   a valid non-inclusive ending span offset.
    | AfterLineEnd
    deriving (Eq, Ord, Show)

-- | Line and col are zero-based, col is in characters.
lineColToByteOffsetDetail
    :: OffsetTable -> Int -> Int
    -> Either OffsetError Int
lineColToByteOffsetDetail OffsetTable{..} line col = do
    (_, lineByteOffs)   <- note NoSuchLine (lineOffsets UV.!? line)
    (charCount, multis) <- note NoSuchLine (lineCharInfo V.!? line)
    unless (charCount > 0) (Left EmptyLine)
    let fileOffset  = bytesUntilCharPosition col multis + lineByteOffs
    unless (col < charCount) $
        let kind = if col == charCount then JustAtLineEnd else AfterLineEnd
        in Left $! OverLineEnd fileOffset kind
    return $! fileOffset
  where
    note :: e -> Maybe a -> Either e a
    note e = maybe (Left e) Right

-- | Like 'lineColToByteOffsetDetail', but without failure details.
lineColToByteOffset :: OffsetTable -> Int -> Int -> Maybe Int
lineColToByteOffset t l c = case lineColToByteOffsetDetail t l c of
    Left _  -> Nothing
    Right a -> Just a

-- | Byte count up to, but excluding the given char position (zero-based).
bytesUntilCharPosition :: Int -> UV.Vector MultibyteChar -> Int
bytesUntilCharPosition n ms
    | UV.null ms = n  -- Most of the cases should hit this shortcut.
    | otherwise =
        let multis = UV.map snd . UV.takeWhile ((< n) . fst) $ ms
            !res = n - UV.length multis + UV.sum multis
        in res

-- | Positions and bytecounts of multibyte characters of a text.
multibytes :: TL.Text -> UV.Vector MultibyteChar
multibytes =
    UV.fromList . reverse . snd . TL.foldl' addMulti (0, [])
  where
    addMulti
        :: (CharOffset, [MultibyteChar]) -> Char
        -> (CharOffset, [MultibyteChar])
    addMulti (coffs, acc) c =
        let !n = charBytes c
            !acc1 | n == 1 = acc
                  | otherwise = (coffs, n):acc
            !coffs1 = coffs + 1
        in (coffs1, acc1)

-- | Based on 'Data.Text.Internal.Encoding.Utf8'.
charBytes :: Char -> Int
charBytes = ordBytes . ord
  where
    ordBytes c
        | c < 0x80    = 1
        | c < 0x0800  = 2
        | c < 0x10000 = 3
        | otherwise   = 4
