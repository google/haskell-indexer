{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module TypeFam where

import Data.Kind (Type)
import GHC.TypeNats (Nat)

-- - @FooClosed defines/binding ClosedFamFoo
type family FooClosed (a :: Nat) where
  -- - @FooClosed ref ClosedFamFoo
  FooClosed 4 = Int
  -- - @FooClosed ref ClosedFamFoo
  FooClosed a = Bool

-- - @FooClosed ref ClosedFamFoo
type F'ooClosed4 = FooClosed 4
