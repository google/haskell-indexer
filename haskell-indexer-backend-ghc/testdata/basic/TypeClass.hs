module TypeClass where

class Foo a where
    foo :: a -> String
    bar :: a

instance Foo Int where
    foo = show
    bar = 42

f x = foo x

g :: Int -> String
g = foo

h :: Int
h = bar
