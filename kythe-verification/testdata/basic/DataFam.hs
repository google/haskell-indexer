{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module DataFam where

import Data.Kind (Type)
import GHC.TypeNats (Nat)

-- - @FooData defines/binding DataFamFoo
data family FooData (a :: Nat) :: Type

-- - @FooData ref DataFamFoo
data instance FooData 4 = F'ooData4 Int

-- - @FooData ref DataFamFoo
type F'ooData4 = FooData 4
