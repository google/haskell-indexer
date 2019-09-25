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

data FooBar
  = FooBar
      { fbFoo :: Int,
        fbBar :: Double
      }
