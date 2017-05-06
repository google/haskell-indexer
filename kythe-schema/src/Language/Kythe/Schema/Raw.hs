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

{-| Raw Kythe schema, suitable for emitting from an analyser.

See http://www.kythe.io/docs/kythe-storage.html and
https://kythe.io/docs/schema/writing-an-indexer.html#_modeling_kythe_entries.
The values in this structure are semantically restricted by the Kythe schema
http://www.kythe.io/docs/schema/. Note: see the head of the github repo for the
latest schema docs, as it is still in flux a bit.

This module is suggested to be imported qualified, and most client code rather
use the safer 'Language.Kythe.Schema.Typed' module.
-}
module Language.Kythe.Schema.Raw
    ( Entry(..)
    , Edge(..), Fact(..)
    , EdgeName, FactName, FactValue
    , module Language.Kythe.Schema.Raw.VName
    ) where

import Data.Text (Text)
import Data.ByteString (ByteString)

import Language.Kythe.Schema.Raw.VName (VName(..))

-- | Top-level Kythe analyser artifact.
data Entry
    = EdgeE !Edge
    | FactE !Fact

-- | Glorified pair of strings, in the context of a given node (identified by
-- its VName).
data Fact = Fact !VName !FactName !FactValue

-- | Directed edge between nodes.
data Edge = Edge !VName !EdgeName !VName  -- TODO(robinpalotai): ordinal

type EdgeName = Text
type FactName = Text
type FactValue = ByteString
