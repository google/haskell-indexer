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

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

-- | Module trying to expose a unified (or at least simplified) view of the GHC
-- AST changes across multiple compiler versions.
module Language.Haskell.Indexer.Backend.Compat where

import Control.Arrow ((&&&))

#if __GLASGOW_HASKELL__ >= 800
import Module (UnitId, unitIdString)
import qualified Bag
#else
import Module (Module, packageKeyString, modulePackageKey)
#endif

#if __GLASGOW_HASKELL__ < 802
import HsDecls (hs_instds)
#endif

#if __GLASGOW_HASKELL__ < 800
import GHC (PackageKey)
import SrcLoc (combineSrcSpans)
#endif

import HsBinds (HsBindLR(..), Sig(..), LHsBinds, abe_mono, abe_poly)
import HsDecls (ConDecl(..), TyClDecl(ClassDecl, DataDecl, SynDecl))
import HsExpr (HsExpr(..), HsRecordBinds)
import qualified HsTypes
import HsTypes (HsType(HsTyVar), LHsType)
import Id (Id)
import Name (Name)
import Outputable (Outputable)
import SrcLoc (Located, GenLocated(L), unLoc, getLoc)

#if __GLASGOW_HASKELL__ >= 800
showPackageName :: UnitId -> String
showPackageName = unitIdString
#else
showPackageName :: PackageKey -> String
showPackageName = packageKeyString
-- | Backfilling.
moduleUnitId :: Module -> PackageKey
moduleUnitId = modulePackageKey
#endif

-- | In GHC before 8.0.1 less things had Located wrappers, so no-op there.
-- Drops the Located on newer GHCs.
#if __GLASGOW_HASKELL__ >= 800
mayUnLoc :: Located a -> a
mayUnLoc = unLoc
#else
mayUnLoc :: a -> a
mayUnLoc = id
#endif

#if __GLASGOW_HASKELL__ < 802
-- | Backfilling.
hsGroupInstDecls = hs_instds
#endif

pattern RecordConCompat :: Located Id -> HsRecordBinds Id -> HsExpr Id
pattern RecordConCompat lConId recBinds <-
#if __GLASGOW_HASKELL__ >= 800
    RecordCon lConId _ _ recBinds
#else
    RecordCon lConId _ recBinds
#endif

pattern DataDeclCompat locName binders defn <-
#if __GLASGOW_HASKELL__ >= 802
    DataDecl locName binders _ defn _ _
#elif __GLASGOW_HASKELL__ >= 800
    DataDecl locName binders defn _ _
#else
    DataDecl locName binders defn _
#endif

pattern SynDeclCompat locName binders <-
#if __GLASGOW_HASKELL__ >= 802
    SynDecl locName binders _ _ _
#else
    SynDecl locName binders _ _
#endif

pattern FunBindCompat funId funMatches <-
#if __GLASGOW_HASKELL__ >= 800
    FunBind funId funMatches _ _ _
#else
    FunBind funId _ funMatches _ _ _
#endif

pattern TypeSigCompat names ty <-
#if __GLASGOW_HASKELL__ >= 800
    TypeSig names ty
#else
    TypeSig names ty _
#endif

#if __GLASGOW_HASKELL__ >= 800
-- | Monomorphising type so uniplate is happier.
namesFromHsIbSig :: HsTypes.LHsSigType Name -> [Name]
namesFromHsIbSig = HsTypes.hsib_vars

namesFromHsWC :: HsTypes.LHsWcType Name -> [Name]
namesFromHsWC = HsTypes.hswc_wcs

namesFromHsIbWc :: HsTypes.LHsSigWcType Name -> [Name]
namesFromHsIbWc =
    -- No, can't use the above introduced names, because the types resolve
    -- differently here. Type-level functions FTW.
#if __GLASGOW_HASKELL__ >= 802
    HsTypes.hswc_wcs
#else
    HsTypes.hsib_vars
#endif
#endif

data ClsSigBound = forall a. Outputable a => ClsSigBound ![Located Name] a

clsSigBound (TypeSigCompat ns ty) = Just (ClsSigBound ns ty)
#if __GLASGOW_HASKELL__ >= 800
clsSigBound (ClassOpSig _ ns ty) = Just (ClsSigBound ns ty)
#endif
-- TODO(robinpalotai): PatSynSig
clsSigBound _ = Nothing

pattern ClassDeclCompat locName binders sigs <-
#if __GLASGOW_HASKELL__ >= 802
    ClassDecl _ locName binders _ _ sigs _ _ _ _ _
#else
    ClassDecl _ locName binders _ sigs _ _ _ _ _
#endif

#if __GLASGOW_HASKELL__ >= 800
conDeclNames (ConDeclH98 conName _ _ _ _) = [conName]
conDeclNames (ConDeclGADT conNames _ _) = conNames
#else
conDeclNames (ConDecl conNames _ _ _ _ _ _ _) = conNames
#endif

data AbsBindsKind = NormalAbs | SigAbs
    deriving (Eq)

maybeAbsBinds :: HsBindLR a b
              -> Maybe (LHsBinds a, [(a, Maybe a)], AbsBindsKind)
maybeAbsBinds (AbsBinds _ _ exports _ binds) =
    let ids = map (abe_poly &&& (Just . abe_mono)) exports
    in Just $! (binds, ids, NormalAbs)
#if __GLASGOW_HASKELL__ >= 800
maybeAbsBinds (AbsBindsSig _ _ poly _ _ bind) =
    let binds = Bag.unitBag bind
        ids = [(poly, Nothing)]
    in Just $! (binds, ids, SigAbs)
#endif
maybeAbsBinds _ = Nothing

pattern AbsBindsCompat binds ids abskind <-
    (maybeAbsBinds -> Just (binds, ids, abskind))

-- | Represents various spans of 'instance' declarations separately.
data SplitInstType = SplitInstType
    { onlyClass :: !Name
    , classAndInstance :: !(LHsType Name)
      -- ^ The location is properly set to the span of 'Cls Inst'
    }

#if __GLASGOW_HASKELL__ >= 800
mySplitInstanceType :: HsTypes.LHsSigType Name -> Maybe SplitInstType
mySplitInstanceType ty = do
    let (_, body) = HsTypes.splitLHsForAllTy (HsTypes.hsSigType ty)
    clsName <- HsTypes.getLHsInstDeclClass_maybe ty
    Just $! SplitInstType
        { onlyClass = unLoc clsName
        , classAndInstance = body
        }
#else
mySplitInstanceType :: LHsType Name -> Maybe SplitInstType
mySplitInstanceType ty = do
    (_, _, L clsL clsName, instLTys) <- HsTypes.splitLHsInstDeclTy_maybe ty
    let clsInstTy = HsTypes.mkHsAppTys (L clsL (HsTypes.HsTyVar clsName))
                                       instLTys
        combinedLoc = foldr (combineSrcSpans . getLoc) clsL instLTys
    Just $! SplitInstType
        { onlyClass = clsName
        , classAndInstance = L combinedLoc clsInstTy
        }
#endif

#if __GLASGOW_HASKELL__ >= 802
hsTypeVarName :: HsType Name -> Maybe (Located Name)
hsTypeVarName (HsTyVar _ n) = Just $! n
#elif __GLASGOW_HASKELL__ >= 800
hsTypeVarName :: HsType Name -> Maybe (Located Name)
hsTypeVarName (HsTyVar n) = Just $! n
#else
hsTypeVarName :: HsType Name -> Maybe Name
hsTypeVarName (HsTyVar n) = Just $! n
#endif
hsTypeVarName _ = Nothing

