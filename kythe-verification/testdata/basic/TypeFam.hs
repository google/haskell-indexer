{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module TypeFam where

import Data.Kind (Type)
import GHC.TypeNats (Nat)

-- - @Foo defines/binding FamFoo
-- - @n defines/binding _
-- - @Nat ref _
-- - @Type ref _
type family Foo (n :: Nat) :: Type

-- It'd be nice to have the following setup, mirroring C++ template
-- specialization:
--
-- * @Foo defines/binding FooFam
-- * FooFam.type abs
-- type family Foo a (n :: Nat) :: Type
--
-- * @Foo defines/binding FooInt4
-- * FooInt4 specializes AppFooInt4
-- * AppFooInt4.node/kind tapp
-- * AppFooInt4 param.0 FooFam
-- * AppFooInt4 param.1 Four
-- * Four.node/kind constant
-- * Four.text 4
-- type instance Foo Int 4 = Bool
--
-- * @Foo ref FooInt4
-- type F'oo4 = Foo Int 4
--
-- * @Foo defines/binding Foo_5
-- type instance Foo a 5 = a
--
-- * @Foo ref FooString5
-- * @String ref StringTy
-- type F'oo5 = Foo String 5
--
-- * FooString5 instantiates AppFoo_5String
-- * AppFoo_5String param.0 Foo_5
-- * AppFoo_5String param.1 StringTy
--
-- * FooString5 specializes AppFooString5
-- * AppFooString5 param.0 FooFam
-- * AppFooString5 param.1 StringTy
-- * AppFooString5 param.2 _

-- Instead we have that instance declarations reference the family, for now.

-- - @Foo ref FamFoo
type instance Foo 4 = Int

-- - @Foo ref FamFoo
type F'oo4 = Foo 4
