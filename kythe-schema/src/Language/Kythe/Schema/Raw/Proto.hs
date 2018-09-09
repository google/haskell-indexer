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
-- | Serializes the raw schema types to proto equivalent.
module Language.Kythe.Schema.Raw.Proto
    ( toEntryProto
    ) where

import Data.Default (def)
import Lens.Family2 ((&), (.~))

import qualified Language.Kythe.Schema.Raw as Raw
-- TODO(robinpalotai): adapt path for open-source release.
import qualified Proto.Kythe.Proto.Storage as K
import qualified Proto.Kythe.Proto.Storage_Fields as K

toEntryProto :: Raw.Entry -> K.Entry
toEntryProto (Raw.EdgeE (Raw.Edge srcVName edgeName targetVName)) = def
    & K.source   .~ toVNameProto srcVName
    & K.edgeKind .~ edgeName
    & K.target   .~ toVNameProto targetVName
    & K.factName .~ "/"  -- expected by Kythe tools.
toEntryProto (Raw.FactE (Raw.Fact vname factName factValue)) = def
    & K.source    .~ toVNameProto vname
    & K.factName  .~ factName
    & K.factValue .~ factValue

toVNameProto :: Raw.VName -> K.VName
toVNameProto (Raw.VName sig corpus root path lang) = def
    & K.signature .~ sig
    & K.corpus    .~ corpus
    & K.root      .~ root
    & K.path      .~ path
    & K.language  .~ lang
