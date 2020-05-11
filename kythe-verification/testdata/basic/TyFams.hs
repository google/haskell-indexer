{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module TyFams where

import Data.Kind (Type)
import GHC.TypeNats (Nat)

-- Note: apparently we don't expose a tokenizer, so the verifier doesn't
-- understand that e.g. "Foo" in "Foo4" is not a full token; so, we obfuscate
-- the matches we don't want with an a'postrophe rather than using @#1 syntax,
-- as the style guide says, "It is better to change code snippets than to use
-- complicated offset specifications."

-- - @Foo defines/binding FamFoo
-- - @n defines/binding _
-- - @Nat ref _
-- - @Type ref _
type family Foo (n :: Nat) :: Type

-- - @Foo ref FamFoo
type F'oo4 = Foo 4

-- - @FooData defines/binding DataFamFoo
data family FooData (a :: Nat) :: Type

-- - @FooData ref DataFamFoo
type F'ooData4 = FooData 4

-- - @FooClosed defines/binding ClosedFamFoo
type family FooClosed (a :: Nat) where

-- - @FooClosed ref ClosedFamFoo
type F'ooClosed4 = FooClosed 4

class Bar a where
  -- - @Baz defines/binding FamBaz
  type Baz a :: Type

-- - @Baz ref FamBaz
type B'azInt = Baz Int
