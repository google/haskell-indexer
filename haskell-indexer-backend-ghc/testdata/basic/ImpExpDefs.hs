module ImportDefs
  ( FooBar (MkFooBar, fbFoo, fbBar),
    bar,
    foo,
  )
where

foo :: Int
foo = 42

bar :: Double
bar = 42.0

data FooBar
  = MkFooBar
      { fbFoo :: Int,
        fbBar :: Double
      }
