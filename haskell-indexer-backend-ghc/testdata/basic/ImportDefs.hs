module ImportDefs
  ( FooBar (..),
    bar,
    foo,
  )
where

foo :: Int
foo = 42

bar :: Double
bar = 42.0

data FooBar
  = FooBar
      { fbFoo :: Int,
        fbBar :: Double
      }
