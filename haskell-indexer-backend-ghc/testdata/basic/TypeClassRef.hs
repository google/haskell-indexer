module TypeClassRef where

called x = x

class Foo a where
    foo :: a -> Int

instance Foo () where
    foo _ = called 1
