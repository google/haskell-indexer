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

{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL

import Test.Framework (defaultMain)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit
import Test.QuickCheck

import Data.Char (chr)
import Data.Maybe (fromJust)
import Data.Text.Offset

main :: IO ()
main = defaultMain
    [ testProperty "bytecount matches final offset"
        propBytecountMatchesFinalOffset
    , testCase "column over end of line" testColOverEndOfLine
    , testCase "column over end of file" testColOverEndOfFile
    , testCase "line over end of file" testLineOverEndOfFile
    , testCase "multibytes" testMultiBytes
    ]

-- | String with more multibytes (Arbitrary for String gives you at most
-- 2-bytes).
newtype MultiString = MultiString { unMultiString :: String }
    deriving (Eq, Show)

instance Arbitrary MultiString where
    arbitrary = MultiString `fmap` listOf genChar
      where
        genChar :: Gen Char
        genChar = fmap chr . frequency $
          [ (8, choose(0, 0x7f))
          , (1, choose(0x80, 0x7ff))
          , (1, choose(0x800, 0xffff))
          , (1, choose(0x10000, 0x10fff))  -- subset of remaining range.
          ]

propBytecountMatchesFinalOffset :: [MultiString] -> Property
propBytecountMatchesFinalOffset mss =
    let ts = map (TL.pack . unMultiString) mss
        -- | Getting back the last valid source offset is not straightforward,
        -- since we don't consider newlines valid source positions. So rather
        -- just make user we don't have a terminal newline, and cover that case
        -- in other tests.
        t = TL.intercalate "\n" ts `TL.append` "guard"
        table = createOffsetTable t
        tlines = TL.splitOn "\n" t
        queryLine = length tlines - 1
        queryCol = (fromIntegral . TL.length . last $ tlines) - 1
        afterLastByteOffset =
            fromJust $ (+1) `fmap` lineColToByteOffset table queryLine queryCol
        encodedLength = fromIntegral . BL.length . TL.encodeUtf8 $ t
        tl = fromIntegral . TL.length $ t
    in classify (length tlines == 1) "single-line" $
       classify (length tlines > 1) "multi-line" $
       classify (encodedLength > tl) "multibyte" $
       classify (encodedLength == tl) "singlebyte" $
       afterLastByteOffset === encodedLength

testColOverEndOfLine :: Assertion
testColOverEndOfLine = do
    let t = createOffsetTable "012\n345"
    assertEqual "valid offset" (Just 2) (lineColToByteOffset t 0 2)
    assertEqual "offset at end" (Left (OverLineEnd 3 JustAtLineEnd))
                                (lineColToByteOffsetDetail t 0 3)
    assertEqual "offset after end" (Left (OverLineEnd 4 AfterLineEnd))
                                   (lineColToByteOffsetDetail t 0 4)

testColOverEndOfFile :: Assertion
testColOverEndOfFile = do
    let t = createOffsetTable "012\n345"
    assertEqual "valid offset before EOF" (Just 6)
                                          (lineColToByteOffset t 1 2)
    assertEqual "invalid offset over EOF" (Left (OverLineEnd 7 JustAtLineEnd))
                                          (lineColToByteOffsetDetail t 1 3)

testLineOverEndOfFile :: Assertion
testLineOverEndOfFile = do
    assertEqual "single line, no newline" (Left NoSuchLine)
        (lineColToByteOffsetDetail (createOffsetTable "012") 1 0)
    assertEqual "single line, with newline" (Left EmptyLine)
        (lineColToByteOffsetDetail (createOffsetTable "012\n") 1 0)
    assertEqual "negative test: non-empty newline" (Just 4)
        (lineColToByteOffset (createOffsetTable "012\n0") 1 0)

testMultiBytes :: Assertion
testMultiBytes = do
    let t = createOffsetTable "0む4\nΣΣ8"
    assertEqual "before 3-byte" (Just 1) (lineColToByteOffset t 0 1)
    assertEqual "after 3-byte" (Just 4) (lineColToByteOffset t 0 2)
    assertEqual "after 2x2-byte last line" (Just 10)
                                           (lineColToByteOffset t 1 2)
