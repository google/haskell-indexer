{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module AssocType where

import Data.Kind (Type)

class Bar a where
  -- - @Baz defines/binding FamBaz
  type Baz a :: Type

instance Bar Int where
  -- - @Baz ref FamBaz
  type Baz Int = Bool

-- - @Baz ref FamBaz
type B'azInt = Baz Int
