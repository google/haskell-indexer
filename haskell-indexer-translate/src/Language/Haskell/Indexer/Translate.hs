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

{-| Intermediate data structure from GhcAnalyser.
This will be transformed to actual Grok/Kythe schema.

We need to keep this relatively lightweight to avoid mirroring GHC AST
structures. The goal for now should be support for code browsing and
crossreferencing, _not_ faithfully mirroring exact types for static analysis.

Should not expose info about the producer (GHC), to make them switchable.

Should not make assumptions about the consuming backend for same reason.
-}
module Language.Haskell.Indexer.Translate
    ( SourcePath(..)
    , Pos(..), Span(..), spanFile
    , Tick(..)
    --
    , XRef(..)
    , AnalysedFile(..)
    , ModuleTick(..)
    , Decl(..)
    , DeclExtra(..), emptyExtra, withExtra
    , PkgModule(..)
    , StringyType(..)
    , TickReference(..), ReferenceKind(..)
    , Relation(..), RelationKind(..)
    ) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)

-- | Like 'FilePath', but uses 'Text' and is a real wrapper, not a type alias.
-- Note: source paths usually live in a temporary workdir, or maybe under some
-- top-level dir. If possible, feed this top-level dir to the frontend, and
-- it should strip it from the emitted paths.
newtype SourcePath = SourcePath { unSourcePath :: Text }
    deriving (Eq, Ord)

instance Show SourcePath where
  show (SourcePath p) = show p

-- | Line, col and filename. Col is in characters (not bytes).
data Pos = Pos !Int !Int !SourcePath
    deriving (Eq, Ord, Show)

-- | A text range with start and end position (inclusive/exclusive
-- respectively).
data Span = Span !Pos !Pos
    deriving (Eq, Ord, Show)

-- | File containing Span.
spanFile :: Span -> SourcePath
spanFile (Span (Pos _ _ f) _) = f

-- | All data for a given input file. TODO rename?
-- Contains lists, to give lazy evaluation a chance and results can eventually
-- be streamed with lower peek memory residency.
data XRef = XRef
    { xrefFile      :: !AnalysedFile
    , xrefModule    :: !ModuleTick
    , xrefDecls     :: [Decl]
    , xrefCrossRefs :: [TickReference]
    , xrefRelations :: [Relation]
    }
    deriving (Eq, Show)

data AnalysedFile = AnalysedFile
    { analysedTempPath :: !SourcePath
      -- ^ The path of the analysed file on the actual filesystem, not
      --   yet stripped of the temporary directory prefix.
      --   The frontend can use this path to read the file contents if needed.
    , analysedOriginalPath :: !SourcePath
      -- ^ A nice, abstract path, which developers think of as the location of
      --   the file. Ideally stripped of temporary workdirs.
    }
    deriving (Eq, Show)

-- | Info
data ModuleTick = ModuleTick
    { mtPkgModule :: !PkgModule
    , mtSpan      :: !(Maybe Span)
      -- ^ Span of the module name.
      -- For example, 'X' in 'module X where'.
      -- Main modules can have this missing.
    }
    deriving (Eq, Show)

-- | A Tick(et) is a globally unique identifier for some entity in the AST.
-- Not mandatory, but is ideally stable across multiple compilations.
--
-- Not related to GHC's SCC annotations (called ticks internally).
data Tick = Tick
    { tickSourcePath :: !SourcePath
    , tickPkgModule  :: !PkgModule
    , tickThing      :: !Text
      -- ^ The unqualified name of the entity.
    , tickSpan :: !(Maybe Span)
      -- ^ Can be broader or just loosely connected to the physical location
      --   of the entity in source. Should only be used for generating a unique
      --   tick string. Use other spans in Decl for source linking.
    , tickUniqueInModule :: !Bool
      -- ^ If true, the generated unique name can omit the span.
      --   This usually signals top levelness too.
      --   TODO(robinpalotai): make the distinction clear? Rename?
    , tickTermLevel :: !Bool
      -- ^ Needed to disambiguate same name occuring in term and type level.
    }
    deriving (Eq, Ord, Show)

data PkgModule = PkgModule
    { getPackage :: !Text
    , getModule :: !Text
    }
    deriving (Eq, Ord, Show)

data Decl = Decl
    { declTick :: !Tick
    , declIdentifierSpan :: !(Maybe Span)
      -- ^ Points to a potentially narrow span containing the identifier of
      --   the decl. Also see other spanny fields in DeclExtra.
    , declType :: !StringyType
      -- ^ Should go away once we switch to emitting separate type Decls.
    , declExtra :: !(Maybe DeclExtra)
      -- ^ Since rarely present, in 'Maybe' to minimize memory usage, and to
      --   let DeclExtra grow without concern.
    }
    deriving (Eq, Show)

-- | Additional information about the decl, for info that is rarely present.
data DeclExtra = DeclExtra
    { methodForInstance :: !(Maybe Text)
      -- ^ A readable unqualified name of the instance, in the form of
      --   "Cls Inst".  Frontends can use this data to provide more descripive
      --   identifier name ("Cls Inst.foo" instead just "foo"), which is
      --   helpful when listed in an UI.
    , alternateIdSpan :: !(Maybe Span)
      -- ^ Set if the declIdentifierSpan overlaps with other spans, making it
      --   problematic for UI tools. Then the alternateIdSpan can be used by
      --   frontends for example for hyperlinking.
    }
    deriving (Eq, Show)

emptyExtra :: DeclExtra
emptyExtra = DeclExtra Nothing Nothing

-- | Modifies declExtra of the given decl, and creates one if it is Nothing so
-- far.
withExtra :: (DeclExtra -> DeclExtra) -> Decl -> Decl
withExtra f d =
    let extra = fromMaybe emptyExtra (declExtra d)
    in d { declExtra = Just $! f extra }

-- | Ideally types of things are referred using the tick of those types.
-- But for complex types, such a tick (and the accompanying decl) must be
-- assembled (recursively) on the spot. This is not yet done, in that case
-- we lie that this is just a simple type (represented by the given string).
-- The loss with that is that the type graph is unusable for programmatic
-- queries.
-- TODO(robinpalotai): remove this and add a proper Decl for the type +
--   a RelationKind ctor 'IsTypeOf'.
data StringyType = StringyType
    { declQualifiedType :: !Text
    , declUserFriendlyType :: !Text
    }
    deriving (Eq, Show)

-- | Reference to the given tick from the given span.
data TickReference = TickReference
    { refTargetTick       :: !Tick
    , refSourceSpan       :: !Span
      -- ^ The precise location of the reference. Frontends probably want to
      --   make this a hyperlink on the UI.
    , refHighLevelContext :: !(Maybe Tick)
      -- ^ The context from which the reference originates. Theoretically a
      --   frontend could infer the context (enclosing scope) from the reference
      --   source span, but 1) it is not obvious how large context to choose,
      --   and 2) since the compiler already has the scoping info, it is easier
      --   for the indexer to emit it.
      --
      --   Here we pragmatically set the context to the current top-level
      --   function, if any. On the UI, this might show up as the next element
      --   in the call chain - see 'ReferenceKind'.
    , refKind             :: !ReferenceKind
    } deriving (Eq, Show)

-- | Distinguishing a plain reference from a call is traditional in imperative
-- languages, but in a functional language - or an imperative one with
-- functional elements - these concepts are fuzzy. For example, we might see
-- partial application as either reference or call.

-- Disclaimer: author of this comment is not an expert on the topic, so
-- following might be imprecise or even foolish.
--
-- Traditionally a call puts a new entry on the call stack.
-- But with partial application or lazy evaluation our runtime representation
-- is a call graph (with thunks as nodes) rather than a linear stack.
--
-- So here we instead resort to considering what a call means for the user.
-- An UI with code crossref support can typically display the call stack or
-- possible call chain of a function. This chain is expanded by the user to
-- discover sites that deal with that function - an example situation can be
-- debugging a problem or understanding new code.
--
-- Here we adopt a simplistic distinction - call is anything with at least one
-- argument application, the rest are reference. The frontend is free to
-- disregard this information and treat everything as calls or references
-- though.
data ReferenceKind
    = Ref      -- ^ Reference
    | Call     -- ^ Function call
    | TypeDecl -- ^ Usage of identifier in type declaration, left to "::"
    deriving (Eq, Ord, Show)

-- | A Relation is between standalone semantic nodes, in contrast to
-- TickReference, which is between a source span and a semantic node.
--
-- Read it aloud as 'relSourceTick' 'relKind' 'relTargetTick'.
data Relation = Relation
    { relSourceTick :: !Tick
    , relKind       :: !RelationKind
    , relTargetTick :: !Tick
    }
    deriving (Eq, Show)

data RelationKind = ImplementsMethod | InstantiatesClass
    deriving (Eq, Ord, Show)


-- | An unqualified id for the purpose of displaying to users.
-- No newtype for this, as we don't do anything with it apart from emitting.
type DisplayId = Text
