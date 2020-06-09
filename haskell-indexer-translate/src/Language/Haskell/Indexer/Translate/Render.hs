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

{- Utilities for rendering entities from Translate into strings.
   Frontends can use this module as a convenience, but it is not mandatory to
   stick to the formats if they don't want.
-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Language.Haskell.Indexer.Translate.Render
    ( tickQualifiedThing
    , tickPredictableQualifiedName
    , makeDeclIdentifier
    , declPreferredUiSpan
    ) where

import Control.Applicative ((<|>))
import Control.Monad (guard)
#if !MIN_VERSION_base(4,11,0)
import Data.Monoid ((<>))
#endif
import Data.Text (Text)

import Language.Haskell.Indexer.Translate

-- | For informal use only (displaying on UI).
tickQualifiedThing :: Tick -> Text
tickQualifiedThing Tick{..} =
    let local = if not tickUniqueInModule then ".<local>" else ""
    in pmText tickPkgModule <> local <> "." <> tickThing

-- | A predictable (expectedly stable across reindexings) qualified name.
-- Only present for top-level things.
tickPredictableQualifiedName :: Tick -> Maybe Text
tickPredictableQualifiedName Tick{..} = do
    guard tickUniqueInModule
    return $! pmText tickPkgModule <> "." <> tickThing

-- | Creates an informal unqualified name for the Decl suitable for UI display.
-- Prefixes instance methods with the instance name for readability.
makeDeclIdentifier :: Decl -> Text
makeDeclIdentifier d =
    let ident = tickThing (declTick d)
    in case declExtra d >>= methodForInstance of
           Just instName -> instName <> "." <> ident
           Nothing -> ident

-- | UIs may use this span as a main anchor for the decl - for hyperlinking
-- referents and other properties.
declPreferredUiSpan :: Decl -> Maybe Span
declPreferredUiSpan decl =
    (declExtra decl >>= alternateIdSpan) <|> declIdentifierSpan decl

pmText :: PkgModule -> Text
pmText pm = getPackage pm <> ":" <> getModule pm
