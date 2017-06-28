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
{-# LANGUAGE RecordWildCards #-}
module Language.Haskell.Indexer.Translate.Utils
    ( spanTickFragment  -- TODO(robinpalotai): rename?
    , tickString
    ) where

import Control.Monad (guard)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Lazy.Builder.Int as TB

import Language.Haskell.Indexer.Translate
    ( SourcePath(..), Span(..), Pos(..), Tick(..), PkgModule(..) )

-- | Returns a fragment usable in a TickString for a given Span.
-- Includes char-based line/col positions and the file name.
--
-- Example output: some/path/to.hs:(3,1)-(3,4)
spanTickFragment :: Span -> T.Text
spanTickFragment (Span (Pos l1 c1 path) (Pos l2 c2 _)) =
    TL.toStrict . TB.toLazyText . mconcat $
        [ TB.fromText (unSourcePath path)
        , ":(", TB.decimal l1, ",", TB.decimal c1
        , ")-(", TB.decimal l2, ",", TB.decimal c2, ")"
        ]

-- | Assuming that package+module name are unique, returns a unique string for
-- a Tick.
tickString :: Tick -> T.Text
tickString Tick{..} = T.intercalate ":" $
    [ "haskell"
    , if tickTermLevel then "term" else "type"
    , getPackage tickPkgModule
    , getModule tickPkgModule
    , tickThing
    ]
    ++ (guard (not tickUniqueInModule) >>
            [ maybe "?" spanTickFragment tickSpan ])
