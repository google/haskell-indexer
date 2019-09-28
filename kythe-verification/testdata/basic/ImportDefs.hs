module ImportDefs
  ( FooBar (..),
    bar,
    foo,
  )
where

foo :: Int
-- - @foo defines/binding FooVar
foo = 42

bar :: Double
-- - @bar defines/binding BarVar
bar = 42.0

-- - @FooBar defines/binding TypeD
-- - @MkFB defines/binding CtorD
-- - @fbFoo defines/binding FieldFbFoo
data FooBar = MkFB {fbFoo :: Int}
