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

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
-- | Converts the entities from the common Translate representation to Kythe
-- entries.
module Language.Haskell.Indexer.Frontend.Kythe
    ( toKythe
    ) where

import Control.Monad.Identity (Identity)
import Control.Monad.Morph (lift, hoist)
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import qualified Data.ByteString as BS
import Data.Conduit (ConduitM, Source)
import Data.Conduit.List (sourceList)
import Data.Maybe (fromMaybe, maybeToList)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Offset as Offset

import Language.Kythe.Schema.Typed
import qualified Language.Kythe.Schema.Raw as Raw
import Language.Kythe.Schema.Raw.VName

import Language.Haskell.Indexer.Translate
import Language.Haskell.Indexer.Translate.Render
import Language.Haskell.Indexer.Translate.Utils (tickString)

-- | Data commonly needed while converting Analysis result to Kythe.
data ConversionEnv = ConversionEnv
    { fileVName   :: !Raw.VName
    , offsets     :: !Offset.OffsetTable
    , baseVName   :: !Raw.VName
    }

-- | The computation context used by the Kythe conversion.
type Conversion = Reader ConversionEnv
type ConversionT = ReaderT ConversionEnv

-- | Converts crossreference data of a file to Kythe schema.
toKythe :: Raw.VName -> T.Text -> XRef -> Source Identity Raw.Entry
toKythe basevn content XRef{..} = do
    let NameAndEntries filevn fileEntries =
            makeFileFacts basevn
                          (analysedOriginalPath xrefFile)
                          encodedContent
        -- TODO(robinpalotai): emit package node, put in env for decls to use.
        env = ConversionEnv filevn table basevn
    sourceList fileEntries
    flip runReaderT env $ do
        mapM_ (stream . makeDeclFacts) xrefDecls
        mapM_ (stream . makeUsageFacts) xrefCrossRefs
        mapM_ (stream . makeRelationFacts) xrefRelations
  where
    table = Offset.createOffsetTable (TL.fromStrict content)
    encodedContent = T.encodeUtf8 content

-- | Changes a conversion returning a list of entries into one that streams
-- those entries. To be applied at reasonable points in the computation tree,
-- where already a decent but not too large amount of entries have accumulated
stream :: Conversion [Raw.Entry]
       -> ConversionT (ConduitM () Raw.Entry Identity) ()
stream a = do
    es <- hoist lift a
    lift (sourceList es)

-- | Glorified pair. The entries are not exhaustive for the given VName.
data NameAndEntries = NameAndEntries !Raw.VName ![Raw.Entry]

-- | Creates file node entries. Not in Conversion, since the result is needed
-- to construct the ConversionEnv.
makeFileFacts :: Raw.VName -> SourcePath -> BS.ByteString -> NameAndEntries
makeFileFacts basevn origPath encodedContent =
      NameAndEntries filevn facts
    where
      facts = nodeFacts filevn FileNK
          [ nodeFact FileTextF encodedContent
          ]
      filevn = basevn
          -- Kythe conventions for file node.
          { vnLanguage  = ""
          , vnSignature = ""
          , vnPath      = unSourcePath origPath
          }

-- | Makes entries for an anchor referring some existing declaration.
makeUsageFacts :: TickReference -> Conversion [Raw.Entry]
makeUsageFacts TickReference{..} = do
    -- TODO(robinpalotai): add more data from backend to translate layer about
    --   spans of ref vs call. In Kythe a call needs both a ref (with name
    --   anchor) and ref/call (with anchor spanning also the args).
    targetVname <- tickVName refTargetTick
    mbCallContext <- traverse tickVName refHighLevelContext
    let edgeType =
          case refKind of
            Ref -> RefE
            Call -> RefE
            TypeDecl -> RefDocE
    makeAnchor (Just refSourceSpan) edgeType targetVname Nothing mbCallContext

-- | Makes all entries for a declaration.
makeDeclFacts :: Decl -> Conversion [Raw.Entry]
makeDeclFacts decl@Decl{..} = do
    declVName <- tickVName declTick
    -- TODO(robinpalotai): use actual node type (Variable is a catch-all now).
    -- TODO(robinpalotai): emit type entries.
    let declFacts = nodeFacts declVName VariableNK
                        [ nodeFact CompleteF Definition ]
    anchorEntries <- makeAnchor (declPreferredUiSpan decl)
                         DefinesBindingE declVName
                         Nothing  -- snippet
                         -- TODO(robinpalotai): plumb high-level context to
                         -- Decl entries too in backend.
                         Nothing
    return (declFacts ++ anchorEntries)

-- | Makes entries for an anchor (either explicit or implicit)..
--
-- If snippet span is not provided, the Kythe service will auto-construct one.
makeAnchor
    :: Maybe Span       -- ^ Anchor span. If missing, anchor is implicit.
    -> AnchorEdge       -- ^ How it refers its target.
    -> Raw.VName        -- ^ Anchor target.
    -> Maybe Span       -- ^ Snippet span.
    -> Maybe Raw.VName  -- ^ Reference context (like enclosing function).
    -> Conversion [Raw.Entry]
makeAnchor mbSource anchorEdge targetVName mbSnippet mbRefContext = do
    (vname, spanOrSubkindFacts) <- fromMaybeTPureDef implicits $ do
        -- TODO(robinpalotai): CPP - spanAndOffs can be Nothing, if a macro
        --   was expanded, since GHC expands the macro on the same, "long" line
        --   with columns over the true columns of that line - and so
        --   OffsetTable will indicate failure. Could workaround and emit anchor
        --   at the beginning of the line, or more properly (?) generate small
        --   virtual files for the macro expansion.
        SpanAndOffs source offs <- spanAndOffs mbSource
        lift $ do
            anchorVN <- anchorVName (spanFile source) offs
                        targetVName
            let spanFacts = makeLocFacts offs
            return (anchorVN, spanFacts)
    snippetFacts <- fromMaybeTPureDef [] $
        makeSnippetFacts . onlyOffset <$> spanAndOffs mbSnippet
    let nodeEntries = nodeFacts vname AnchorNK . concat $
            [ spanOrSubkindFacts
            , snippetFacts
            ]
    childOfFile <- edge vname ChildOfE <$> asks fileVName
    let edgeEntries =
            [ childOfFile
            , edge vname (AnchorEdgeE anchorEdge) targetVName
            ] ++ maybeToList (edge vname ChildOfE <$> mbRefContext)
    return $! nodeEntries ++ edgeEntries
  where
    implicits = ( implicitAnchorVName targetVName
                , [nodeFact AnchorSubkindF ImplicitAnchor] )

-- | Makes fact about a non-reference edge.
makeRelationFacts :: Relation -> Conversion [Raw.Entry]
makeRelationFacts (Relation src relKind target) =
    traverse (\k -> edge <$> tickVName src <*> pure k <*> tickVName target)
             edgeKinds
  where
    edgeKinds = case relKind of
         ImplementsMethod  -> [OverridesE, OverridesRootE]
         InstantiatesClass -> [ExtendsE]
           -- ^ Note: In Haskell-terms, class instantiation is not extension
           --   (or subclassing), but in Kythe terms we can think of the class
           --   as an interface, and the instance as the implementation.
           --   So by squinting a bit, we'll hopefully get better functionality
           --   out of Kythe-related tools that are not aware of Haskell.

-- Helpers below.

-- | Makes a location start/end fact pair using the given offset.
makeLocFacts :: OffsetRange -> [AnonymousFact]
makeLocFacts (OffsetRange s e) =
    [ nodeFact LocStartF s
    , nodeFact LocEndF e
    ]

-- | Makes a snippet start/end fact pair using the given offset.
makeSnippetFacts :: OffsetRange -> [AnonymousFact]
makeSnippetFacts (OffsetRange s e) =
    [ nodeFact SnippetStartF s
    , nodeFact SnippetEndF e
    ]

-- | Generates VName for an abstract entity.
-- The generated name uniquely identifies the given entitiy.
tickVName :: Tick -> Conversion Raw.VName
tickVName t = updateSig <$> asks baseVName
  where updateSig v = v { vnSignature = tickString t }

-- | A non-implicit anchor VName.
-- In theory the pure location is enough to construct it, but for practical
-- debuggability we stick the signature of the target entity to the signature
-- of the generated VName.
anchorVName :: SourcePath -> OffsetRange -> Raw.VName -> Conversion Raw.VName
anchorVName sourcePath (OffsetRange s e) targetVName =
    update <$> asks baseVName
  where
    update v = v
        { vnPath = unSourcePath sourcePath
        , vnSignature = vnSignature targetVName
                      <> ":" <> tshow s <> "," <> tshow e
        }

-- | An implicit anchor doesn't have a source location, but we still need to
-- make a unique VName for it. So we generate it from the signature of the
-- target.
implicitAnchorVName :: Raw.VName -> Raw.VName
implicitAnchorVName targetVName = targetVName
    { vnSignature = vnSignature targetVName <> ":ImplicitAnchor" }

-- Dealing with spans and offsets.

-- | Glorified tuple.
data SpanAndOffs = SpanAndOffs !Span !OffsetRange

-- | Glorified snd.
onlyOffset :: SpanAndOffs -> OffsetRange
onlyOffset (SpanAndOffs _ offs) = offs

-- | High-level helper to get span info with less boilerplate.
spanAndOffs :: Maybe Span -> MaybeT Conversion SpanAndOffs
spanAndOffs mbSpan = do
    aSpan <- hoistMaybe mbSpan
    offs <- MaybeT (spanToOffsets aSpan)
    return $! SpanAndOffs aSpan offs

-- | A range in bytes.
data OffsetRange = OffsetRange !Int !Int

-- | Translates char-based span to byte-based offsets, using the table
-- constructed for the analysed file in context.
spanToOffsets :: Span -> Conversion (Maybe OffsetRange)
spanToOffsets (Span (Pos l1 c1 _) (Pos l2 c2 _)) = do
    t <- asks offsets
    let start = Offset.lineColToByteOffset t (l1 - 1) (c1 - 1)
        -- Being just at line end is acceptable for end span.
        end = case Offset.lineColToByteOffsetDetail t (l2 - 1) (c2 - 1) of
            Right o                                          -> Just o
            Left (Offset.OverLineEnd o Offset.JustAtLineEnd) -> Just o
            Left _                                           -> Nothing
    return $! OffsetRange <$> start <*> end

-- Boilerplate usually found in upstream libs.

-- | Like 'show' but return 'Text' instead 'String'.
tshow :: (Show a) => a -> Text
tshow = T.pack . show

-- | Shorthand for MaybeT . return.
hoistMaybe :: (Monad m) => Maybe a -> MaybeT m a
hoistMaybe = MaybeT . return

-- | Like 'fromMaybe', but for 'MaybeT' and with a pure default.
fromMaybeTPureDef :: (Monad m) => a -> MaybeT m a -> m a
fromMaybeTPureDef a = fmap (fromMaybe a) . runMaybeT
