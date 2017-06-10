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

{-| Convenience module for building Kythe facts and edges.
Provides slightly more safety over the raw structures in 'Kythe.Schema.Raw'
by using enumerations and types.

The set of available nodes/edges is not exhaustive, and only concerned about
encoding the ones needed for indexing Haskell. Once the Kythe schema is
finalized, the module can be refactored to full support.

Note: during development, more aggressive type restrictions were considered,
but the power-to-code ratio was inadequate. Mostly due to the Kythe analysis
happening in a distributed way, so full info about a target node is not
available.
-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Kythe.Schema.Typed
    ( fact
    , Fact(..)
    , AnonymousFact
    -- Nodes
    , NodeKind(..)
    , nodeFacts, nodeFact
    -- Facts
    , AnchorSubkind(..)
    , HowComplete(..)
    -- Edges
    , edge
    , Edge(..), AnchorEdge(..)
    ) where

import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)
import Data.ByteString (ByteString)

import qualified Language.Kythe.Schema.Raw as Raw

-- | Node kinds allowed by schema.
data NodeKind
    = AnchorNK
    | FileNK
    | VariableNK

-- | Renders node kind to Kythe text value.
printNodeKind :: NodeKind -> Text
printNodeKind = \case
    AnchorNK   -> "anchor"
    FileNK     -> "file"
    VariableNK -> "variable"

-- | Name of a node fact constrained by the accepted value type.
--
-- There can be multiple, node kind dependent fact names referring to the same
-- raw Kythe fact name, if the value type can differ depending on the node kind.
-- This is most notably true for the "text" fact.
data Fact value where
    AnchorSubkindF :: Fact AnchorSubkind
    NodeKindF      :: Fact NodeKind
    FileTextF      :: Fact ByteString
    LocStartF      :: Fact Int
    LocEndF        :: Fact Int
    CompleteF      :: Fact HowComplete
    SnippetStartF  :: Fact Int
    SnippetEndF    :: Fact Int

-- | Renders a fact name to Kythe text value.
printFact :: Fact v -> Text
printFact = \case
    AnchorSubkindF -> "/kythe/subkind"
    NodeKindF      -> "/kythe/node/kind"
    FileTextF      -> "/kythe/text"
    LocStartF      -> "/kythe/loc/start"
    LocEndF        -> "/kythe/loc/end"
    CompleteF      -> "/kythe/complete"
    SnippetStartF  -> "/kythe/snippet/start"
    SnippetEndF    -> "/kythe/snippet/end"

-- | How complete is the declaration / definition.
data HowComplete
    = Incomplete  -- ^ Incomplete, can't be used yet.
    | Complete    -- ^ Can be used, but not fully defined.
    | Definition  -- ^ Fully defined.

-- | Anchors can be implicit.
data AnchorSubkind = ImplicitAnchor

-- | Constructs an emittable raw fact.
--
-- The types don't protect against using the wrong fact for a given node kind.
-- That was considered but too noisy to be practical on small scale usage.
fact :: (FactValuePrintable v) => Raw.VName -> Fact v -> v -> Raw.Fact
fact vname f v = Raw.Fact vname (printFact f) (printFactValue v)

-- | Convenience for describing all facts about a node. Saves passing the vname
-- arg each time, compared to 'fact'. Also automatically adds the node kind
-- fact.
--
-- Example usage:
--
--     nodeFacts vname FileNK [ nodeFact FileTextF contentBytes, ... ]
--
nodeFacts :: Raw.VName -> NodeKind -> [AnonymousFact] -> [Raw.Entry]
nodeFacts vname nk = map (Raw.FactE . go) . (nkFact:)
  where
    go (AnyFact rf rv) = Raw.Fact vname rf rv
    nkFact = nodeFact NodeKindF nk

-- | To be used with 'nodeFacts'.
nodeFact :: (FactValuePrintable v) => Fact v -> v -> AnonymousFact
nodeFact f v = AnyFact (printFact f) (printFactValue v)

-- | Glorified pair.
data AnonymousFact = AnyFact !Raw.FactName !Raw.FactValue

-- | Describes how to render fact values to raw Kythe bytes.
class FactValuePrintable a where
    printFactValue :: a -> Raw.FactValue

instance FactValuePrintable ByteString where
    printFactValue = id

instance FactValuePrintable Text where
    printFactValue = encodeUtf8

instance FactValuePrintable NodeKind where
    printFactValue = encodeUtf8 . printNodeKind

instance FactValuePrintable Int where
    -- TODO(robinpalotai): BS Builder
    printFactValue = encodeUtf8 . pack . show

instance FactValuePrintable HowComplete where
    printFactValue = \case
        Incomplete -> "incomplete"
        Complete   -> "complete"
        Definition -> "definition"

instance FactValuePrintable AnchorSubkind where
    printFactValue = \case
        ImplicitAnchor -> "implicit"

-- | Emits an edge entry. To be used directly.
edge :: Raw.VName -> Edge -> Raw.VName -> Raw.Entry
edge source e target = Raw.EdgeE (Raw.Edge source (printEdge e) target)

data Edge
    = ChildOfE
    | OverridesE
    | OverridesRootE
    | ExtendsE   -- ^ Note: Kythe allows emitting "extends/"-prefixed custom
                 --   edges. No need for that, yet.
    | AnchorEdgeE AnchorEdge

-- | Edges running from anchor to semantic node.
data AnchorEdge
    = DefinesE
    | DefinesBindingE
    | RefE
    | RefCallE
    | RefDocE

printEdge :: Edge -> Text
printEdge = \case
    ChildOfE        -> "/kythe/edge/childof"
    OverridesE      -> "/kythe/edge/overrides"
    OverridesRootE  -> "/kythe/edge/overrides/root"
    ExtendsE        -> "/kythe/edge/extends"
    AnchorEdgeE e   -> case e of
        DefinesE        -> "/kythe/edge/defines"
        DefinesBindingE -> "/kythe/edge/defines/binding"
        RefE            -> "/kythe/edge/ref"
        RefCallE        -> "/kythe/edge/ref/call"
        RefDocE         -> "/kythe/edge/ref/doc"

