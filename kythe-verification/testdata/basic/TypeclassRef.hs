-- | Haskell's "instance of class" relation is mapped to "extends" in the Kythe
-- schema. The choice is somewhat arbitrary, but enables nice interop with
-- Kythe-based tooling. Mapping visually (and a bit superficially):
--
--     Haskell class    ~ interface
--     Haskell instance ~ (singleton-instantiated) implementation
--
module TypeClass where
{-# ANN module "HLint: ignore Eta reduce" #-}

-- - @Foo defines/binding ClassFoo
class Foo a where
    -- TODO(robinpalotai): - FunFoo childof ClassFoo
    -- - @foo defines/binding FunFoo
    foo :: a -> String

-- For UI convenience, we emit the binding site to the 'instance' keyword, since
-- it is easier to click than say spaces in the instance head.
-- - @instance defines/binding InstIntFoo
-- - @Foo ref ClassFoo
-- - InstIntFoo extends ClassFoo
instance Foo Int where
    -- TODO(robinpalotai): - FunIntFoo childof InstIntFoo
    -- - @foo defines/binding FunIntFoo
    -- - FunIntFoo overrides FunFoo
    -- - FunIntFoo overrides/root FunFoo
    -- - @show childof FunIntFoo
    foo = show

-- - @foo ref FunFoo
f x = foo x

g :: Int -> String
-- TODO(robinpalotai): - @foo ref FunIntFoo, instead of the below, when we know
-- the precise instance (here it is coerced by the type signature).
-- - @foo ref FunFoo
g = foo
