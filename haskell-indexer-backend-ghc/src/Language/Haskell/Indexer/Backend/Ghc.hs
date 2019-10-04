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

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-| Converts GHC typechecked AST into intermediate Analysed format.

This module should not assume anything about the backend consuming the
Analysed format (so various backends can be plugged).
-}
module Language.Haskell.Indexer.Backend.Ghc
    ( analyseTypechecked
    , analyseTypechecked'
    , generateRefEdgeForHiddenImports
    , AnalysisOptions(..)
    ) where

-- GHC API imports
import Avail (Avails)
-- TODO(robinpalotai) qualify these
import qualified Bag as GHC
import qualified BasicTypes as GHC
import qualified ConLike
import qualified DataCon as GHC
import qualified Name as GHC
import FastString (unpackFS)
import FieldLabel (FieldLbl (..))
import GHC
import qualified Id as GHC
import qualified HsTypes
import Module (unitIdString)
import Name (nameModule_maybe, nameOccName)
import qualified Outputable as GHC
import OccName (occNameString)
import SrcLoc (realSrcSpanStart, realSrcSpanEnd)
import Var (Var, varName, varType)

import Control.Arrow ((&&&), (***), second)
import Data.Bool (bool)
import Data.Data (Data)
import Data.Either (partitionEithers)
import Data.Function (on)
import Data.List (partition, sortBy, groupBy)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromJust, fromMaybe, mapMaybe, maybeToList)
import Data.Monoid ((<>))
import Data.Ord (comparing)
import Data.Reflection (Given, give, given)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (Typeable)
import Network.URI (escapeURIString, isUnescapedInURIComponent)

import Language.Haskell.Indexer.Backend.AnalysisOptions
import Language.Haskell.Indexer.Backend.GhcEnv (GhcEnv(..))
import Language.Haskell.Indexer.Backend.GhcLens (universeBi, children)
import Language.Haskell.Indexer.Translate

{- TODO(robinpalotai): List of main outstanding items.

  * Definition <-> Declaration pointers.

  * CPP
    - Handle when entities are found on lines that were affected by CPP.
    - Other preprocessors.

  * Type families and pals.

  * Documentation nodes.
    - Attached to the decls.

  * Import/Exports.
    - Spans where things are exported/imported from/to the module.
-}

-- | Data commonly accessed during analysis.
-- TODO(robinpalotai): now this is passed sometimes directly sometimes with
--   reflection. Should make it consistent.
data ExtractCtx = ExtractCtx
    { ecModule     :: !Module
    , ecSourcePath :: !SourcePath  -- ^ Stripped.
    , ecGhcEnv     :: !GhcEnv
    , ecOptions    :: !AnalysisOptions
    }

analyseTypechecked :: GhcMonad m => GhcEnv -> AnalysisOptions -> TypecheckedModule -> m XRef
analyseTypechecked ghcEnv opts tm =
  let modSummary = pm_mod_summary . tm_parsed_module $ tm
      renSource = tm_renamed_source tm  :: Maybe RenamedSource
      tcSource = tm_typechecked_source tm
      moduleSpan =
        fmap getLoc . hsmodName . unLoc . pm_parsed_source
          $ tm_parsed_module tm
      modInfo = (ms_mod modSummary, moduleSpan)
  in analyseTypechecked' ghcEnv opts modSummary renSource tcSource modInfo

analyseTypechecked'
  :: GhcMonad m => GhcEnv -> AnalysisOptions
  -> ModSummary -> Maybe RenamedSource -> LHsBinds GhcTc
  -> (Module, Maybe SrcSpan)
  -> m XRef
analyseTypechecked' ghcEnv opts modSummary renSource tcSource (mod', mspan) = do
  let ctx = ExtractCtx (ms_mod modSummary) strippedModFile ghcEnv opts
      moduleTick = give ctx $ mkModuleTick mspan mod'
      -- Analysed modules always arrive as file references in practice.
      modFile = T.pack $!
                    fromMaybe "?" (ml_hs_file . ms_location $ modSummary)
      strippedModFile = SourcePath (aoFilePathTransform opts modFile)
      declsAlts =
          let (fromRenamed, declMods) =
                  fromMaybe mempty (declsFromRenamed ctx <$> renSource)
              fromTypechecked = declsFromTypechecked ctx tcSource declMods
          in fromRenamed ++ fromTypechecked
      altMap = declAltMap declsAlts
      refs = tcRefs ++ renamedRefs
        where
          tcRefs = refsFromTypechecked ctx tcSource altMap
          renamedRefs = fromMaybe [] (refsFromRenamed ctx altMap <$> renSource)
      rels = fromMaybe [] (relationsFromRenamed ctx altMap <$> renSource)
      decls = map daDecl declsAlts
      docDecls = generateDocDecls refs
  imports <- fromMaybe (return []) (importsFromRenamed ctx <$> renSource)
  return $ XRef
      { xrefFile = (AnalysedFile (SourcePath modFile) strippedModFile)
      , xrefModule = moduleTick
      , xrefDecls = decls
      , xrefDocDecls = docDecls
      , xrefCrossRefs = refs
      , xrefRelations = rels
      , xrefImports = imports
      }
  where
    declAltMap :: [DeclAndAlt] -> DeclAltMap
    declAltMap = M.fromList . mapMaybe toPair
      where
        toPair (DeclAndAlt d (Just alt)) = Just (alt, declTick d)
        toPair _ = Nothing

-- | Generate decls that point to hackage documents.
-- These decls are mainly for things from GHC core packages. Their source code
-- is not available but we can make them point to their documents.
generateDocDecls :: [TickReference] -> [Decl]
generateDocDecls refs =
  -- Deduplicate by URI strings.
  let uriTickMap =
        foldr
          ( \r acc ->
              let targetTick = refTargetTick r
               in case tickDocUri targetTick of
                    Nothing -> acc
                    Just uri -> M.insert uri targetTick acc
          )
          M.empty
          refs
   in map
        ( \(uri, tick) ->
            -- Generate "fake" decls.
            Decl
              { declTick = tick,
                declIdentifierSpan = Nothing,
                declType = StringyType
                  { declQualifiedType = uri,
                    declUserFriendlyType = uri
                  },
                declExtra = Nothing
              }
        )
        (M.toList uriTickMap)

-- | Bundles a declaration with an alternative reference, if any.
-- The alternate reference can be used by other AST elements to refer to the
-- element, but we want to redirect that reference to our main tick in Decl.
data DeclAndAlt = DeclAndAlt
    { daDecl :: !Decl
    , _daAlt :: !(Maybe Tick)
      -- ^ Alternative reference, recursive functions can be referred using
      --   this - see https://ghc.haskell.org/trac/ghc/ticket/11176.
      --   There are other use cases too - see usages.
    }
    deriving (Eq, Show)

mapDeclAndAlt :: (Decl -> Decl) -> DeclAndAlt -> DeclAndAlt
mapDeclAndAlt f (DeclAndAlt d a) = DeclAndAlt (f d) a

setIdSpan :: Maybe Span -> Decl -> Decl
setIdSpan s d = d { declIdentifierSpan = s }

-- | Maps from alternative ticket to preferred - see DeclAndAlt.
type DeclAltMap = M.Map Tick Tick

altTickToPrimary :: DeclAltMap -> Tick -> Tick
altTickToPrimary declAlts t = M.findWithDefault t t declAlts

-- | Describes modifications that should be applied to certain declarations.
-- If possible, rather emit these as Tick-based relations, since then the
-- frontend can (hopefully) emit the relation without looking up the Decl.
-- Only use DeclMod if it is important to have some additional info on the Decl
-- so the frontend can output it accurately.
data DeclMod
    = MethodForInstance !Text
      -- ^ For a typeclass instance, the full information is split between the
      --   RenamedSource and the TypecheckedSource - the Decl will be extracted
      --   from the TypecheckedSource, but the RenamedSource knows the
      --   instance info, such as the name of the instance.

type DeclMods = M.Map Tick DeclMod
                           -- ^ Change to list if we have more DeclMod ctors?

-- | Applies modifications for the given Decl, if any.
modifyDecl :: DeclMods -> Decl -> Decl
modifyDecl declMods decl =
    maybe decl applyMod . M.lookup (declTick decl) $ declMods
  where
    applyMod (MethodForInstance i) =
        withExtra (\e -> e {methodForInstance = Just $! i}) decl

-- | Bundles up the module name with its source span.
mkModuleTick :: (Given ExtractCtx) => Maybe SrcSpan -> Module -> ModuleTick
mkModuleTick span' mod' = ModuleTick pkgmodule moduleNameSpan
  where
    moduleNameSpan = srcSpanToSpan =<< span'
    pkgmodule = extractModuleName given mod'


hsTypeVarName :: HsType GhcRn -> Maybe (Located Name)
hsTypeVarName (HsTyVar _ _ n) = Just $! n
hsTypeVarName _ = Nothing


-- | Extracts:
--   * datatypes, constructors, type variable bindings.
--   * type class declaration with method signatures
--     TODO(robinpalotai): what about bodies of default methods?
--                         Are those in the typechecked AST?
declsFromRenamed :: ExtractCtx -> RenamedSource -> ([DeclAndAlt], DeclMods)
declsFromRenamed ctx (hsGroup, _, _, _) =
    let defs = hs_tyclds hsGroup >>= group_tyclds >>= dataDecls
        (instDefs, instChanges) = second M.unions . unzip . mapMaybe instDecls
                                . hsGroupInstDecls
                                $ hsGroup
        explicitVarBindDefs = map mkDeclName explicitNames
        varBindDefs = explicitVarBindDefs ++ implicitVarBindDefs
    in (concat [defs, instDefs, varBindDefs], instChanges)
  where
    keepFirst [] = Nothing
    keepFirst xs@((name, _):_) = Just $! (name, minimum $ map snd xs)
    mkDeclName n = nameDeclAlt ctx n Nothing typeStringyType
    explicitNames = concatMap namesFromForall . universeBi $ hsGroup
    implicitVarBindDefs =
        let -- Collect the usages so we can assign a binding at the first usage.
            -- Note: we could also just omit the location, making it an implicit
            -- decl. Or put the location as an alternate span.
            allUsages = mapMaybe hsTypeVarName . universeBi $ hsGroup
            firstUsages = mapMaybe keepFirst
                . groupBy ((==) `on` fst)
                . sortBy (comparing fst)
                . map fromLoc
                $ allUsages
            implicitDefs = Set.fromList . concat $
                [ concatMap namesFromHsIbSig . universeBi $ hsGroup
                , concatMap namesFromHsIbWc . universeBi $ hsGroup
                , concatMap namesFromHsWC . universeBi $ hsGroup
                ]
        in map mkDecl . filter (\(n,_) -> Set.member n implicitDefs)
           $ firstUsages
      where
        mkDecl (n,l) = declWithWrappedIdLoc typeStringyType (L l n)
        fromLoc (L l n) = (n, l)
    -- | Monomorphising type so uniplate is happier.
    namesFromHsIbSig :: HsTypes.LHsSigType GhcRn -> [Name]
    namesFromHsIbSig = hsib_vars . HsTypes.hsib_ext

    namesFromHsWC :: HsTypes.LHsWcType GhcRn -> [Name]
    namesFromHsWC = HsTypes.hswc_ext

    namesFromHsIbWc :: HsTypes.LHsSigWcType GhcRn -> [Name]
    namesFromHsIbWc =
        -- No, can't use the above introduced names, because the types resolve
        -- differently here. Type-level functions FTW.
        HsTypes.hswc_ext
    --
    namesFromForall :: HsType GhcRn -> [Name]
    namesFromForall (HsForAllTy _ binders _) = map hsLTyVarName binders
    namesFromForall _ = []
    -- | For typevar binders directly attached to datatype declarations (like
    -- data, type, class etc), the location of the name is correctly set to
    -- the span of the type variable.
    -- TODO(robinpalotai): as in declsFromForall about stripping kind signature
    --   from span.
    hsqShim = hsq_explicit
    declsFromDataBinders :: LHsQTyVars GhcRn -> [DeclAndAlt]
    declsFromDataBinders = map (mkDecl . hsTyVarBinderName . unLoc) . hsqShim
      where
        mkDecl n = nameDeclAlt ctx n Nothing typeStringyType
    hsTyVarBinderName :: HsTyVarBndr id -> IdP id
    hsTyVarBinderName (UserTyVar _ n) = unLoc n
    hsTyVarBinderName (KindedTyVar _ n _) = unLoc n
    hsTyVarBinderName (XTyVarBndr _) =
        error "should not hit XTyVarBndr when accessing renamed AST"
    -- Datatypes.
    dataDecls :: LTyClDecl GhcRn -> [DeclAndAlt]
    dataDecls (L _ (DataDecl _ locName binders _ defn)) =
        let top = dataCtorLikeDecl locName
            ctors = map (conDecls . unLoc) . dd_cons $ defn
            tyvars = declsFromDataBinders binders
        in top:(ctors ++ tyvars)
      where
        conDecls cd =
            -- TODO(robinp): all ctors
            -- https://github.com/google/haskell-indexer/issues/108
            let conLName = head (conDeclNames cd)
            in dataLikeDecl cd conLName
        conDeclNames (ConDeclH98 { con_name = conName })  = [conName]
        conDeclNames (ConDeclGADT { con_names = conNames }) = conNames
        conDeclNames (XConDecl _) = error "unexpected XConDecl"
    -- Type aliases.
    dataDecls (L _ sd@(SynDecl _ locName binders _ _)) =
        -- For type aliases we use 'dataLikeDecl' to render the full definition
        -- into the docstring, which is usually short and helpful for aliases.
        let alias = dataLikeDecl sd locName
            tyvars = declsFromDataBinders binders
        in alias:tyvars
    -- Typeclasses.
    -- TODO(robinpalotai): functional dependencies.
    dataDecls (L _ (ClassDecl _ _ locName binders _ _ sigs _ _ _ _)) =
        let top = dataCtorLikeDecl locName
            tyvars = declsFromDataBinders binders
            fromSigs = concatMap (sigDecls . unLoc) sigs
        in top:(fromSigs ++ tyvars)
      where
        -- For typeclasses, we emit the declaration from the method name in the
        -- method signature (as opposed to normal functions, where we emit from
        -- the definition's name).
        sigDecls s = case s of
            TypeSig _ lnames ty -> map (dataLikeDecl ty) lnames
            ClassOpSig _ _ lnames ty -> map (dataLikeDecl ty) lnames
            -- TODO(robinpalotai): PatSynSig
            _ -> []
    -- Other
    dataDecls _ = []
    --
    instDecls (L wholeSrcSpan (ClsInstD _ (ClsInstDecl _ lty lbinds _ _ _ _)))
        = do
        -- 1) We emit the instance declaration with the idSpan set to
        --    the "<cls> <inst>" string ad-hocly, since we don't have a better
        --    name. But this can overlap with other name spans (class,
        --    instance, type args..), which can be problematic on a UI, so we
        --    also emit an alternate span for the "instance" string.
        --
        -- 2) We don't emit declarations for the methods, since instance
        --    methods are present in the typechecked source (will emit there).
        --    But we emit an identifier override for the methods so they show
        --    up nice in the call-chain.
        splitType <- mySplitInstanceType lty
        let niceInstIdentifier = T.pack
                               . filterInstanceName
                               . ghcPrintUnqualified (ecGhcEnv ctx)
                               . unLoc . classAndInstance
                               $ splitType
            changes = declChanges niceInstIdentifier
            instTick = makeInstanceTick ctx splitType
            setAltId extra = extra
                { alternateIdSpan = give ctx (instanceKeywordSpan wholeSrcSpan)
                }
            instDecl = withExtra setAltId
                           (tickDecl ctx (outputableStringyType ctx lty) instTick)
        return $! (DeclAndAlt instDecl Nothing, changes)
      where
        declChanges instIdentifier = M.unions
                                     . map (renameInstanceMethod . unLoc)
                                     . GHC.bagToList
                                     $ lbinds
          where
            renameInstanceMethod = \case
                (FunBind _ funName _ _ _) ->
                    let key = makeInstanceMethodTick ctx funName
                        change = MethodForInstance instIdentifier
                    in M.singleton key change
                _ -> M.empty
    instDecls _ = Nothing
    --
    dataLikeDecl :: (GHC.Outputable a) => a -> Located Name -> DeclAndAlt
    dataLikeDecl a = declWithWrappedIdLoc (outputableStringyType ctx a)
    dataCtorLikeDecl = declWithWrappedIdLoc typeStringyType
    declWithWrappedIdLoc declType locatedName =
        let idLoc = getLoc locatedName
            nameWithBroadLoc = unLoc locatedName
        in mapDeclAndAlt (setIdSpan (give ctx $ srcSpanToSpan idLoc))
                         (nameDeclAlt ctx nameWithBroadLoc Nothing declType)

-- | Remove some weird characters from the synthesized name.
filterInstanceName :: String -> String
filterInstanceName = filter (/= '\n')

-- | Gets the span of the "instance" keyword of a typeclass instance
-- definition. Assumes the argument span starts at the "instance" keywork -
-- the full body span of the instance is such.
instanceKeywordSpan :: Given ExtractCtx => SrcSpan -> Maybe Span
instanceKeywordSpan fullSrcSpan = do
    Span p@(Pos line col f) _ <- srcSpanToSpan fullSrcSpan
    Just $! Span p (Pos line (col + 8) f)  -- on "instance"


-- | Represents various spans of 'instance' declarations separately.
data SplitInstType = SplitInstType
    { onlyClass :: !Name
    , classAndInstance :: !(LHsType GhcRn)
      -- ^ The location is properly set to the span of 'Cls Inst'
    }

mySplitInstanceType :: HsTypes.LHsSigType GhcRn -> Maybe SplitInstType
mySplitInstanceType ty = do
    let (_, body) = HsTypes.splitLHsForAllTy (HsTypes.hsSigType ty)
    clsName <- HsTypes.getLHsInstDeclClass_maybe ty
    Just $! SplitInstType
        { onlyClass = unLoc clsName
        , classAndInstance = body
        }

makeInstanceTick :: ExtractCtx -> SplitInstType -> Tick
makeInstanceTick ctx splitType = Tick
    { tickSourcePath = ecSourcePath ctx
    , tickPkgModule = extractModuleName ctx (ecModule ctx)
    , tickThing = T.pack . filterInstanceName $!
          ghcPrintUnqualified (ecGhcEnv ctx)
                              (unLoc $ classAndInstance splitType)
      -- Unlike Purescript, instances don't have a name by default, so
      -- we invent one that should be unique in the module.
    , tickSpan = give ctx ((srcSpanToSpan . getLoc . classAndInstance) splitType)
    , tickUniqueInModule = False
      -- Could be True, but let's keep False, which is more in line with AST.
    , tickTermLevel = False
    , tickDocUri = Nothing
    }

-- | Whether to generate a Ref edge from hidden imported entities to their
-- definition sites.
generateRefEdgeForHiddenImports :: Bool
generateRefEdgeForHiddenImports = True

-- | Collects type-level references. These visibly appear in type signatures,
-- which are only present in the renamed tree.
refsFromRenamed :: ExtractCtx -> DeclAltMap -> RenamedSource
                -> [TickReference]
refsFromRenamed ctx declAlts (hsGroup, importDecls, exports, _) =
    let typeRefs = mapMaybe refsFromHsType (universeBi hsGroup)
        -- TODO(robinpalotai): maybe add context. It would need first finding
        --   the context roots, and only then doing the traversal.
        sigRefs = case hs_valds hsGroup of
            ValBinds _ _ _ ->
                error "should not hit ValBinds when accessing renamed AST"
            XValBindsLR (NValBinds _ lsigs) -> concatMap refsFromSignature lsigs
        importRefs = concatMap refsFromImportDecl importDecls
        exportRefs = refsFromExports exports
        refContext = Nothing
    in map (toTickReference ctx refContext declAlts)
           (typeRefs ++ sigRefs ++ importRefs ++ exportRefs)
  where
    refsFromHsType :: LHsType GhcRn -> Maybe Reference
    refsFromHsType (L l ty) = case hsTypeVarName ty of
        -- Basic variable at the leaves of type trees.
        -- Not only "real" type variables, but also type-level terms like
        -- specific types, type aliases etc.
        Just n -> give ctx (nameLocToRef (unLoc n) Ref l)
        -- TODO(robinpalotai): HsTyLit for type literals.
        _ -> Nothing

    refsFromSignature :: LSig GhcRn -> [Reference]
    refsFromSignature (L _ sig) = case sig of
        TypeSig _ names _ ->
            mapMaybe (\(L l n) -> give ctx (nameLocToRef n TypeDecl l)) names
        _ -> []

    refsFromExports :: Maybe [(LIE GhcRn, Avails)] -> [Reference]
    refsFromExports Nothing = []
    refsFromExports (Just exs) = concatMap (refsFromLIE Ref . fst) exs

    refsFromImportDecl :: LImportDecl GhcRn -> [Reference]
    refsFromImportDecl (L _ idecl) = case idecl of
      ImportDecl {..} ->
        case ideclHiding of
          Nothing -> []
          Just (False, (L _ imports)) ->
            concatMap (refsFromLIE Import) imports
          Just (True, (L _ imports))
            | generateRefEdgeForHiddenImports ->
              concatMap (refsFromLIE Ref) imports
            | otherwise -> []
      _ -> []

    refsFromLIE :: ReferenceKind -> LIE GhcRn -> [Reference]
    refsFromLIE refKind (L _ (IEVar _ (L l n))) =
      maybeToList $ give ctx (nameLocToRef (ieWrappedName n) refKind l)
    refsFromLIE refKind (L _ (IEThingAbs _ (L l n))) =
      maybeToList $ give ctx (nameLocToRef (ieWrappedName n) refKind l)
    refsFromLIE refKind (L _ (IEThingAll _ (L l n))) =
      maybeToList $ give ctx (nameLocToRef (ieWrappedName n) refKind l)
    refsFromLIE refKind (L _ (IEThingWith _ (L tl tn) _ ctors fields)) =
      typeRef ++ ctorRefs ++ fieldRefs
      where
        typeRef =
          maybeToList $ give ctx (nameLocToRef (ieWrappedName tn) refKind tl)
        ctorRefs =
          mapMaybe
            ( \(L cl cn) ->
                give ctx (nameLocToRef (ieWrappedName cn) refKind cl)
            )
            ctors
        fieldRefs =
          mapMaybe
            ( \(L fl label) ->
                give ctx (nameLocToRef (flSelector label) refKind fl)
            )
            fields
    refsFromLIE _ _ = []

-- | Exports subclasses/overrides relationships from typeclasses.
relationsFromRenamed :: ExtractCtx -> DeclAltMap -> RenamedSource
                     -> [Relation]
relationsFromRenamed ctx declAlts (hsGroup, _, _, _) =
    let methodOverrides = concatMap overrides . hsGroupInstDecls $ hsGroup
        classInstances = mapMaybe instances . hsGroupInstDecls $ hsGroup
    in map replaceAltTicks (methodOverrides ++ classInstances)
  where
    overrides = \case
        (L _ (ClsInstD _ (ClsInstDecl _ _ lbinds _ _ _ _))) ->
            mapMaybe (methodOverride . unLoc) (GHC.bagToList lbinds)
        _ -> []
      where
        methodOverride = \case
            (FunBind _ clsFunName _ _ _) ->
                let clsFunTick = nameInModuleToTick ctx (unLoc clsFunName)
                    instFunTick = makeInstanceMethodTick ctx clsFunName
                in Just $! Relation instFunTick ImplementsMethod clsFunTick
            _ -> Nothing
    --
    instances = \case
        (L _(ClsInstD _ (ClsInstDecl _ lty _ _ _ _ _))) -> do
            splitType <- mySplitInstanceType lty
            let clsTick = nameInModuleToTick ctx (onlyClass splitType)
                instTick = makeInstanceTick ctx splitType
            Just $! Relation instTick InstantiatesClass clsTick
        _ -> Nothing
    --
    replaceAltTicks (Relation s k t) =
        let fromAlt = altTickToPrimary declAlts
        in Relation (fromAlt s) k (fromAlt t)

-- | Exports module imports.
importsFromRenamed :: GhcMonad m => ExtractCtx -> RenamedSource -> m [ModuleTick]
importsFromRenamed ctx (_, lImportDecls, _, _) = mapM mkImport lImportDecls
  where
    mkImport :: GhcMonad m => LImportDecl GhcRn -> m ModuleTick
    mkImport (L _ implDecl) = do
      pkgModule <- extractPkgModule . unLoc . ideclName $ implDecl
      let pkgSpan = give ctx (srcSpanToSpan . getLoc $ ideclName implDecl)
      return $ ModuleTick pkgModule pkgSpan

    extractPkgModule :: GhcMonad m => ModuleName -> m PkgModule
    extractPkgModule name = findModule name Nothing >>= return . extractModuleName ctx


-- | Fabricates an instance method tick based on RenamedSource data. The
-- fabricated tick should be the same the TypecheckedSource-based declaration
-- will contain.
makeInstanceMethodTick :: ExtractCtx -> Located Name -> Tick
makeInstanceMethodTick ctx (L l classMethod) = Tick
    -- Reuse name from classMethod, but use the outer location and the current
    -- module otherwise.
    { tickSourcePath = ecSourcePath ctx
    , tickPkgModule = extractModuleName ctx (ecModule ctx)
    , tickThing = nameOccurenceText classMethod
    , tickSpan = give ctx (srcSpanToSpan l)
    , tickUniqueInModule = False
    , tickTermLevel = True
    , tickDocUri = Nothing
    }

-- | Returns the (source-based, generated) bindings.
--
-- The matchgroup (~implementation) of compiler-generated bindings should not be
-- extracted, since they contain both artificial references and strange
-- declarations.
--
-- For example, for record constructors, the generated accessor has a reference
-- to the ctor as it needs to pattern match it (surprising but ok), but the
-- pattern match also introduces a pattern whose span is set exactly to that of
-- the accessor method, which is confusing (and overlapping).
--
-- The exported declarations of generated bindings are still useful.
partitionTopLevelBindsByMatchGroupOrigin
    :: TypecheckedSource -> ([LHsBindLR GhcTc GhcTc], [LHsBindLR GhcTc GhcTc])
partitionTopLevelBindsByMatchGroupOrigin =
    partition (isFromSource . unLoc) . GHC.bagToList
  where
    isFromSource = \case
        FunBind _ _ mg _ _ ->
            not . GHC.isGenerated . mg_origin $ mg
        AbsBinds { abs_binds = binds } ->
            -- Practically there will be a FunBind below which holds the truth.
            -- Except typeclass instance methods, which will have an extra
            -- layer of AbsBinds, but the recursion takes care of that.
            goAbs binds
        _ -> True
    --
    goAbs = null . snd . partitionTopLevelBindsByMatchGroupOrigin

declsFromTypechecked :: ExtractCtx -> TypecheckedSource -> DeclMods
                     -> [DeclAndAlt]
declsFromTypechecked ctx tsrc instDeclMods =
    let (src, gen) = partitionTopLevelBindsByMatchGroupOrigin tsrc
        (fromRegularSrc, fromInstanceSrc)
            = both (concatMap (deepDeclsFromTopBind ctx))
            . partitionInstanceAbsBinds
            $ src
        -- Skip compiler-generated bodies, just extract the top-level def.
        generated = declsFromHsBinds ctx . map (ParentChild Nothing) $ gen
    in fromRegularSrc ++ modifyDecls fromInstanceSrc ++ generated
  where
    modifyDecls = map (mapDeclAndAlt (modifyDecl instDeclMods))

-- | Emits function declarations and pattern variables.
deepDeclsFromTopBind :: ExtractCtx -> LHsBindLR GhcTc GhcTc -> [DeclAndAlt]
deepDeclsFromTopBind ctx top =
    let hsbDecls = declsFromHsBinds ctx (universeWithParents top)
        -- ^ Warning, target type same as source, use universe (not Bi).
        patDecls = mapMaybe declsFromPat . universeBi $ top
    in hsbDecls ++ patDecls
  where
    declsFromPat :: LPat GhcTc -> Maybe DeclAndAlt
    declsFromPat (L _ p) = case p of
        -- Eventually every interesting pattern ends in some variable capturing
        -- patterns.
        VarPat _ v -> Just $! varDeclNoAlt (unLoc v)
        -- Below are special patterns that declare things outside an LPat, so
        -- universe traversal wouldn't capture them. We emit declarations for
        -- these special things (Pats are taken care by above case).
        AsPat _ (L _ asVar) _ -> Just $! varDeclNoAlt asVar
        -- Universe covers the rest.
        _ -> Nothing
      where
        varDeclNoAlt v = varDeclAlt ctx v Nothing

data ParentChild a = ParentChild
  { _pcParent :: !(Maybe a)
  , _pcChild :: !a
  }

-- | Like 'universe', but serves the elements along with their nearest ancestor
-- of the same type. The top element will be returned too, but without a parent.
universeWithParents :: (Data a, Typeable a) => a -> [ParentChild a]
universeWithParents a = ParentChild Nothing a : go a
  where
    go x = let cs = children x
           in map (ParentChild (Just $! x)) cs ++ concatMap go cs


absExportsToIds :: [ABExport p] -> [(IdP p, Maybe (IdP p))]
absExportsToIds exports = map (abe_poly &&& (Just . abe_mono)) exports


-- | Function declarations from the bindings.
--
-- There can be overlapping binds in the input, specifically some
-- bindings can have both abstraction and function bindings. So we check for
-- these Abs+Fun bindings, and only emit the polymorphic binding from
-- the Abs, and retarget the monomorphic references to point to the
-- polymorphic.
--
-- Note: normally recursive calls target the monomorphic bind, except if the
-- function has a type signature.
--
-- Note: Typechecked fundecls contain record accessors, but not data
-- constructors - latter are exported from the renamed source.
declsFromHsBinds :: ExtractCtx -> [ParentChild (LHsBind GhcTc)] -> [DeclAndAlt]
declsFromHsBinds ctx = dedupByTick . concatMap go
  where
    go :: ParentChild (GenLocated SrcSpan (HsBindLR GhcTc GhcTc))
                      -> [DeclAndAlt]
    go (ParentChild (Just (L _ (AbsBinds {abs_exports = exports, abs_sig = sig})))
                    (L _ (FunBind _ lVar _ _ _)))
        | sig =
            -- Need special handling, since it AbsBindsSig doesn't expose the
            -- monomorphic binding. So we harvest it from the FunBind below.
            let primary = varDecl ctx . fst . head $ absExportsToIds exports
                          -- head is safe here but ugly. Maybe better pattern?
                alternative = nameInModuleToTick ctx (varName (unLoc lVar))
            in [DeclAndAlt primary (Just $! alternative)]
        | otherwise = []
    go (ParentChild _ c) = absFunDeclsFromHsBind c
    --
    absFunDeclsFromHsBind :: LHsBind GhcTc -> [DeclAndAlt]
    absFunDeclsFromHsBind (L _ b) = case b of
        FunBind _ (L idLoc funVar) _ _ _ ->
            let decl = setIdSpan (give ctx $ srcSpanToSpan idLoc)
                                 (varDecl ctx funVar)
            in [DeclAndAlt decl Nothing]
        AbsBinds {abs_exports = exports } ->
            let abeVar = uncurry (varDeclAlt ctx)
            in map abeVar . filter (not . isInstanceMethodVar . fst)
                  $ absExportsToIds exports
        _ ->
            -- TODO(robinpalotai): anything to do here?
            []
    -- | Deduping is needed since an AbsBindsSig gets the decl emitted twice,
    -- but note that only one of them (the latter, deeper the tree) has an
    -- alternative, which we need. TODO(robinp): this is hacky. Maybe preserve
    -- the explicit tree structure while traversing for efficient dedup.
    dedupByTick :: [DeclAndAlt] -> [DeclAndAlt]
    dedupByTick = map last . groupBy ((==) `on` crit) . sortBy (comparing crit)
      where
        crit = declTick . daDecl

-- | These are special typeclass related bindings which are not
-- needed for us now (and contain overlapping declarations).
isInstanceMethodVar :: Var -> Bool
isInstanceMethodVar v = specialVar v == Just TypeclassySV

specialVar :: Var -> Maybe SpecialVar
specialVar v = case take 2 (nameOccurenceString (varName v)) of
    -- List appended as cases are encountered.
    "$d" -> Just $! TypeclassySV
    "$f" -> Just $! TypeclassySV
    "$c" -> Just $! TypeclassySV
    "$W" -> DataConWorkerWrapperSV <$> GHC.isDataConId_maybe v
    _ -> Nothing

data SpecialVar
    = TypeclassySV
    | DataConWorkerWrapperSV DataCon
    deriving (Eq)

-- | As nameDeclAlt but for Vars. Uses the Var as the source of type info.
varDeclAlt :: ExtractCtx -> Var -> Maybe Var -> DeclAndAlt
varDeclAlt ctx v alt =
    nameDeclAlt ctx (varName v) (FromName . varName <$> alt)
                (outputableStringyType ctx (varType v))

-- | Like nameDecl but for Vars.
varDecl :: ExtractCtx -> Var -> Decl
varDecl ctx v = nameDecl ctx (varName v) (outputableStringyType ctx (varType v))

-- | Defines how to produce an alternate reference target.
data AlternateRefTarget
    = FromName Name
      -- ^ The Name is directly used as the alternate target.

-- | Creates a Decl entry from a named thing, optionally attaching an alternate
-- reference target.
nameDeclAlt
    :: ExtractCtx -> Name -> Maybe AlternateRefTarget -> StringyType
    -> DeclAndAlt
nameDeclAlt ctx n alt t =
    let altTick = applyAlternate <$> alt
          where
            applyAlternate (FromName altName) = nameInModuleToTick ctx altName
    in DeclAndAlt (nameDecl ctx n t) altTick

nameDecl :: ExtractCtx -> Name -> StringyType -> Decl
nameDecl ctx name t = tickDecl ctx t (nameInModuleToTick ctx name)

outputableStringyType :: GHC.Outputable a => ExtractCtx -> a -> StringyType
outputableStringyType ctx a = StringyType
    { declQualifiedType = typeString ghcPrintPrecise
    , declUserFriendlyType = typeString ghcPrintUnqualified
    }
  where
    typeString printer = T.pack . printer (ecGhcEnv ctx) $ a

-- | A catch-all type-of-types.
-- For now we export (visible) type declarations from the renamed tree, where
-- we don't yet have type-checked info (such as kind of type, etc).
-- TODO(robinpalotai): augment the decl with more precise type info based on
-- the typechecked tree.
typeStringyType :: StringyType
typeStringyType = StringyType
    { declQualifiedType = "SomeKind"
    , declUserFriendlyType = "SomeKind"
    }

-- | Creates a Decl from a tick, by default taking id-span and identifier from
-- the tick.
tickDecl :: ExtractCtx -> StringyType -> Tick -> Decl
tickDecl (ExtractCtx _ _ GhcEnv{..} _) stype t = Decl
    { declTick = t
    , declIdentifierSpan = tickSpan t
    , declType = stype
    , declExtra = Nothing
    }

-- | Reference to a name from the given span.
data Reference = Reference !Name !ReferenceKind !Span
    deriving (Eq, Ord)

-- Redirect source (the intermediate var) and final redirect target.
-- TODO(robinpalotai): now we use this to hack around record pattern things,
--   but we should rather emit richer declarations/references here, and let
--   the frontends decide what they keep/refer and how.
--
--   The problem is a balance between accurateness AST-wise, usability of
--   hover/click/backreference behavior on UI, accuracy of displayed types.
--   These can't be uniquely solved in the backend.
--
--   For example, since we now redirect references of local field bindings
--   to the bindings in the record ctor, the type when hovering on such a
--   redirected reference says 'Rec -> FieldType', instead of just 'FieldType'.
--
--   Other example, when we hover the usage of a local binding, ideally we
--   would want both the ctor field and the local pattern binding highlighted,
--   but we don't want to include the local pattern binding in backreferences
--   on the UI when the ctor field is clicked.
--   The two goals are conflicting, at least if the UI is not prepared for such
--   situations. Now we go with more noise in backrefs, since the hovering
--   highlight is useful.
--
--   Wildcarded/punned record write (construction) is especially tricky, since
--   the punned field can be read at multiple locations after assigned, in
--   addition to be consumed by the punned pattern. Ideally we would amend each
--   such usage (and the local binding site) with a reference to the ctor field,
--   but for now we just emit a reference from the binding site to the ctor
--   field, which coincides with the binding (naturally), and doesn't have
--   guaranteed UI behavior. We should rather let the frontend deal with this
--   (for example, it could just emit all if it knows the UI can handle, or
--   could try to break the binding span in two as a workaround, using one
--   half for upreference, and other for the local binding decl).
type Redirect = (Name, Name)
mkRedirect :: Name -> Name -> Redirect
mkRedirect = (,)

-- | If the arg is a top-level AbsBinds for a typeclass instance method,
-- returns the AbsBinds below it - this is an irregularity in the AST.
instanceAbsBinds :: LHsBindLR GhcTc GhcTc -> Maybe [LHsBindLR GhcTc GhcTc]
instanceAbsBinds (L _ AbsBinds {abs_exports = exports, abs_binds = binds})
    | (isInstanceMethodVar . fst) `any` (absExportsToIds exports) =
        Just $! GHC.bagToList binds
instanceAbsBinds _ = Nothing

-- | Pulls the AbsBinds below the top one up (if typeclass instance method), or
-- leaves the original in place otherwise.
pullInstanceAbsBindsToTop :: LHsBindLR GhcTc GhcTc -> [LHsBindLR GhcTc GhcTc]
pullInstanceAbsBindsToTop = (fromMaybe . return) <*> instanceAbsBinds

-- | Returns (non-instance binds, instance-binds).
partitionInstanceAbsBinds
    :: [LHsBindLR GhcTc GhcTc]
    -> ([LHsBindLR GhcTc GhcTc], [LHsBindLR GhcTc GhcTc])
partitionInstanceAbsBinds = both concat . partitionEithers . map instToRight
  where
    instToRight b = maybe (Left [b]) Right . instanceAbsBinds $ b

-- | Gathers term-level references.
refsFromTypechecked :: ExtractCtx -> TypecheckedSource -> DeclAltMap
                    -> [TickReference]
refsFromTypechecked ctx tsrc declAlts =
    let sourceBasedBinds = fst (partitionTopLevelBindsByMatchGroupOrigin tsrc)
    in sourceBasedBinds >>= pullInstanceAbsBindsToTop >>= children
           >>= refsFromBelowTop
    -- TODO(robinpalotai): sort out what happens to files in these spans if
    --   there is a preprocessor.
    --
    -- When file has CPP, span file is the temporary written by GHC.
    -- Need to filter/alter references from these files that come from modified
    -- lines (since spans refer to the ransformed file). Maybe make them
    -- line-level instead of sub-line.
    --
    -- Or does GHC do the above already?
  where
    -- | References from a single binding directly below a top-level AbsBind.
    -- This single binding is used as the context of emitted references, if
    -- suitable - practically FunBind is suitable, PatBind is usually not,
    -- since it is not obvious which part of the pattern should we attribute
    -- references to.
    refsFromBelowTop (L _ subBind) =
        let exprRefs = concatMap refsFromExpr . universeBi $ subBind
            (patRefs, redirs) = unzip . map refsFromPat . universeBi $ subBind
            refContextTick = case subBind of
                FunBind _ (L _  declRef) _ _ _ ->
                    Just $! nameInModuleToTick ctx (varName declRef)
                _ -> Nothing
        in map (toTickReference ctx refContextTick declAlts)
               (postprocess (concat redirs) exprRefs ++ concat patRefs)
    -- | Deal with preprocessed things later.
    --sourceMatches f (Reference _ rs) = spanFile rs == f
    --
    -- | Pattern bindings can also contain references - for example when a data
    -- constructor is pattern matched, the ctor name is referred.
    refsFromPat :: LPat GhcTc -> ([Reference], [Redirect])
    refsFromPat (L _ p) = case p of
        -- ConPatOut (we are after typechecking).
        ConPatOut (L l conLike) _ _ _ _ details _ ->
            let conRef = give ctx (nameLocToRef (getName conLike) Ref l)
                -- TODO(robinpalotai): could add nature to the Ref/Redirect, as
                --   in read/write/update, which could be useful for some UIs.
                recordRefs = refsFromHsConDetails details
                redirects = redirectsFromHsConDetails details
            in (maybeToList conRef ++ recordRefs, redirects)
        -- TODO(robinpalotai): more?
        _ -> ([], [])
    --
    refsFromHsConDetails = \case
        RecCon r -> mapMaybe (refsFromField . unLoc) (rec_flds r)
          where
            refsFromField f =
                let name = recordFieldName f
                    loc = getLoc (hsRecFieldId f)
                in give ctx (nameLocToRef name Ref loc)
        _ -> []
    --
    redirectsFromHsConDetails = \case
        RecCon r -> mapMaybe redirectsFromField (categorizedRecordFields r)
          where
            -- Explicitly assigned fields don't need redirect, since
            -- the assignment is obvious.
            redirectsFromField (ExplicitAssignedRF, _) = Nothing
            -- Wildcard/punned record fields introduce a local var of the
            -- same name as the record field.
            redirectsFromField (_, f) = case hsRecFieldArg f of
                -- We can only redirect patterns which deconstruct into
                -- a single leaf var (otherwise it is ambigous which
                -- deconstructed part to redirect).
                -- TODO(robinpalotai): follow a whitelisted chain of
                --   decorative patterns (BangPat, ParPat etc) too.
                L _ (VarPat _ var) -> Just $!
                    mkRedirect (varName $ unLoc var) (recordFieldName f)
                -- TODO(robinpalotai): could emit a reference from the span
                --   of the complex pattern match too, probably marking it
                --   so frontends that don't handle the overlap well can
                --   discard.
                _ -> Nothing
        -- TODO(robinpalotai): maybe emit references to the anonymous data
        --   fields in the rest of the cases too? This is interesting in
        --   two cases:
        --   1) Record datacon is unpacked in the regular way - we could
        --      emit references to the field labels from the unpack location.
        --   2) Regular datacon is unpacked - we could emit a refernce to
        --      the field - but the field is just marked by it's type, so
        --      this is a potential decl (field)/ref (type of field) overlap.
        _ -> []
    --
    refsFromExpr :: LHsExpr GhcTc -> [Reference]
    refsFromExpr (L l x) = case x of
        -- Eventually all interesting value references point to a variable.
        HsVar _ vid ->
            let n = case specialVar (unLoc vid) of
                    -- See notes in DataCon.hs. We exported the declaration
                    -- from the renamed source, which has the name of the
                    -- DataCon.
                    Just (DataConWorkerWrapperSV dc) -> GHC.dataConName dc
                    _ -> varName (unLoc vid)
            in maybeToList (give ctx (nameLocToRef n Ref l))
        -- Special handling for call-like expressions. We emit an extra ref
        -- with kind Call, then the duplicate Ref produced by the HsVar match
        -- will be removed in 'postprocessCalls'.
        OpApp _ op _ _  -> callRef op
        SectionL _ _ op -> callRef op
        SectionR _ _ op -> callRef op
        HsApp _ f _     -> callRef f
        -- Below experssions contain refs outside of LHsExpr, so traversal
        -- would miss them.
        --
        -- Note: What is an EAsPat, and what is its first id?
        --
        HsWrap _ _ e -> refsFromExpr (L l e) -- 'e' is just HsExpr, not LHsExpr.
        HsConLikeOut _ conLike ->
            -- From GHC 8.2.1, ctor reference is not just a simple HsVar.
            let name = ConLike.conLikeName conLike
            in maybeToList (give ctx (nameLocToRef name Ref l))
        RecordCon _ locDataConId r -> recordConRefs locDataConId r
        -- TODO(robinp): emit ref to all the possible constructors (also below).
        RecordUpd (RecordUpdTc (ConLike.RealDataCon dc:_) _ _ _) _ r ->
            recordUpdRefs dc r
        -- Others handled by universe.
        _ -> []
      where
        recordConRefs (L ctorLoc conId) r =
            let dataCon = GHC.idDataCon conId
                ctorRef = give ctx (
                              nameLocToRef (GHC.dataConName dataCon) Ref ctorLoc)
                fieldNames = GHC.dataConFieldLabels dataCon
                fieldRefs = concatMap (recordFieldRefs fieldNames)
                          . categorizedRecordFields
                          $ r
            in maybeToList ctorRef ++ fieldRefs
        -- | DataCon is any datacon that has all fields that are updated.
        -- The same-named fields of different datacons are actually just a
        -- single field, so it's ok to pass in an arbitrary DataCon to mine
        -- for reference targets.
        recordUpdRefs :: DataCon -> [LHsRecUpdField GhcTc] -> [Reference]
        recordUpdRefs dataCon =
            let fieldNames = GHC.dataConFieldLabels dataCon
            in concatMap (recordFieldRefs fieldNames)
                . map (
                      (ExplicitAssignedRF,)
                        -- We are in Typechecked tree, where ambiguities
                        -- are already resolved. This is safe.
                        . (\(HsRecField a b c) -> HsRecField
                            (unambiguousFieldOcc <$> a) b c)
                        . unLoc
                      )
      -- See comments on 'Redirect' about how this should be in the future.
        recordFieldRefs
            :: [GHC.FieldLabel] -> (RecFieldCat, HsRecField GhcTc (LHsExpr GhcTc))
            -> [Reference]
        recordFieldRefs fieldLabels (cat, f) =
            let fieldNames = map GHC.flSelector fieldLabels
                fieldMap = M.fromList (fieldNames `zip` fieldNames)
                -- For some reason the name in the hsRecFieldId has the span
                -- differently set compared to the datacon field, but the
                -- uniq is the same so name equality will work.
                -- Note: we could rely on that our tick generation would
                -- yield the same tick now (since datacons are external), but
                -- that would be fragile.
                almostFieldName = varName . unLoc . hsRecFieldId $ f
            in -- Not-found case should not happen in practice.
               maybe [] fieldRefs $ M.lookup almostFieldName fieldMap
          where
            fieldRefs target = catMaybes $ case cat of
                ExplicitAssignedRF -> [idWrapRef]
                PunnedRF -> [idWrapRef, argNameRef]
                -- Outputting idWrapRef for wildcard dots is a bit noisy in
                -- backreference listing, but useful when hovering.
                WildCardRF -> [idWrapRef, argNameRef]
              where
                idWrapRef = give ctx (nameLocToRef target Ref
                                          (getLoc (hsRecFieldId f)))
                argNameRef = case hsRecFieldArg f of
                    -- TODO(robinpalotai): maybe follow chain of decorative
                    --   exprs that still have a single var at the leaf.
                    --   Not sure it can happen, test on AST.
                    L _ (HsVar _ v) -> give ctx $
                        nameLocToRef target Ref
                                     (nameSrcSpan . varName . unLoc $ v)
                    _ -> Nothing
        callRef (L appl e) = case e of
            -- Base case, but practically not occuring directly.
            HsVar _ v -> maybeToList $
                give ctx (nameLocToRef (varName (unLoc v)) Call appl)
            -- After typechecking this happens in practice.
            HsWrap _ _ expr -> callRef (L appl expr)
            -- The AST omits parens if precedence doesn't require them, but
            -- adding this just in case.
            HsPar _ lexpr -> callRef lexpr
            _ -> []
    -- | Drops Ref-s which also have a Call emitted, also replaces the reference
    -- targets if a suitable Redirect is found.
    -- TODO(robinpalotai): record the fact in Reference that there was a
    --   redirect, in case it is useful for the frontends?
    postprocess :: [Redirect] -> [Reference] -> [Reference]
    postprocess redirs = map rewriteRef
                       . concatMap dropRefIfHasCall
                       . groupBy ((==) `on` nameSpan)
                       . sortBy (comparing nameSpan)
      where
        redirMap = M.fromList redirs
        nameSpan (Reference n _ s) = (n,s)
        rKind (Reference _ k _) = k
        dropRefIfHasCall :: [Reference] -> [Reference]
        dropRefIfHasCall =
            -- Could be a simple partition, but doing this way to future-proof
            -- in case there would be more ReferenceKinds.
            bool <$> id <*> filter (not . isRef) <*> any isCall
          where
            isCall = (Call ==) . rKind
            isRef = (Ref ==) . rKind
        rewriteRef r@(Reference n k s) = maybe r setTarget (M.lookup n redirMap)
          where
            setTarget t = Reference t k s

-- | Categories a record (de)constructor field can fall into.
data RecFieldCat = ExplicitAssignedRF | PunnedRF | WildCardRF

categorizedRecordFields
    :: HsRecFields id arg
    -> [(RecFieldCat, HsRecField id arg)]
categorizedRecordFields recFields = do
    let explicitCount = rec_dotdot recFields
    (i, field) <- [0..] `zip` (unLoc <$> rec_flds recFields)
    let cat = if hsRecPun field
          then PunnedRF
          else case (i <) <$> explicitCount of
              Just True -> ExplicitAssignedRF
              Just False -> WildCardRF
              Nothing -> ExplicitAssignedRF
    return $! (cat, field)

recordFieldName :: HsRecField GhcTc arg -> Name
recordFieldName = varName . unLoc . hsRecFieldId

toTickReference
    :: ExtractCtx
    -> Maybe Tick  -- ^ The context (usually top-level decl) of the reference.
    -> DeclAltMap -> Reference
    -> TickReference
toTickReference ctx refContext declAlts (Reference name refKind span0) =
    let tick = nameInModuleToTick ctx name
    in TickReference (replaceWithPrimary tick) span0
                     (replaceWithPrimary <$> refContext) refKind
        where
          replaceWithPrimary = altTickToPrimary declAlts

nameLocToRef
    :: Given ExtractCtx
    => Name -> ReferenceKind -> SrcSpan -> Maybe Reference
nameLocToRef n k s = Reference n k <$> srcSpanToSpan s

hackageSrcUrl :: ExtractCtx -> Module -> Name -> Text
hackageSrcUrl ctx m n =
  let pkg = showOutputable ctx $ moduleUnitId m
      modName = showOutputable ctx $ moduleName m
      name = showOutputable ctx $ nameOccName n
   in T.pack
        $ "https://hackage.haskell.org/package/"
          ++ escape pkg
          ++ "/docs/src/"
          ++ escape modName
          ++ ".html#"
          ++ escape name
  where
    escape :: String -> String
    escape = escapeURIString isUnescapedInURIComponent

showOutputable :: GHC.Outputable a => ExtractCtx -> a -> String
showOutputable = ghcPrintUnqualified . ecGhcEnv

{- Note [GHC Names]:

   GHC 'Var's/'Name's have a sort, which is roughly Internal or External.
   External corresponds to top-level in module (be it the current module or
   some other). See the comments in
   https://github.com/ghc/ghc/blob/master/compiler/basicTypes/Name.hs#L142
   for more precise description.

   Importantly, being External/Internal has nothing to do with being exported
   from the module (at least directly after type checking, and maybe before
   core tidy). But External seem to be a good candidate for generating
   simpler, spanless unique names.
-}

srcSpanToSpan :: (Given ExtractCtx) => SrcSpan -> Maybe Span
srcSpanToSpan = \case
    UnhelpfulSpan _ -> Nothing
    RealSrcSpan r -> Just $ Span (realToPos . realSrcSpanStart $ r)
                                 (realToPos . realSrcSpanEnd $ r)
  where
    realToPos :: RealSrcLoc -> Pos
    realToPos r =
        let path = SourcePath . transform . T.pack . unpackFS . srcLocFile $ r
        in Pos (srcLocLine r) (srcLocCol r) path
      where transform = aoFilePathTransform (ecOptions given)

-- | Creates a unique Tick from a Name. The Name need not refer to an entity
-- in the current module, but if the Name's location is Nothing, it is assumed
-- (rightly) that it refers an entity in the current module, so the current
-- module is used in the generated Tick.
nameInModuleToTick :: ExtractCtx -> Name -> Tick
nameInModuleToTick ctx n = Tick
    { tickSourcePath = sourcePath
    , tickPkgModule = extractModuleName ctx nModule
    , tickThing = nameOccurenceText n
    , tickSpan = nameSpan
    , tickUniqueInModule = isExternalName n
    , tickTermLevel = GHC.isValName n
    , tickDocUri = docUri
    }
  where
    nameSpan = give ctx (srcSpanToSpan (nameSrcSpan n))
    nModule = fromMaybe (ecModule ctx) (nameModule_maybe n)
    sourcePath = case nameSpan of
        Nothing -> SourcePath $ fromJust docUri
        Just s -> spanFile s
    docUri = case nameSpan of
        -- An empty name span probably means something from core packages.
        -- Associate with the hackage document.
        Nothing -> Just $ hackageSrcUrl ctx nModule n
        Just _ -> Nothing

nameOccurenceText :: Name -> Text
nameOccurenceText = T.pack . nameOccurenceString

nameOccurenceString :: Name -> String
nameOccurenceString = occNameString . nameOccName

-- | Breaks a GHC Module definition into a package and module name.
extractModuleName :: ExtractCtx -> Module -> PkgModule
extractModuleName ctx m =
    let opts = ecOptions ctx
        ps = aoDemanglePackageName opts . T.pack
           . unitIdString
           . moduleUnitId
           $ m
        ms = (T.pack . moduleNameString . moduleName) m
        p = if ps == "main" then aoMainPkgFallback opts <> "_main" else ps
    in PkgModule p ms

both :: (a -> b) -> (a,a) -> (b,b)
both f = f *** f
