{-# LANGUAGE TypeOperators #-}

module TypeOperator where

data a + b = Sum a b

mkSum :: a -> b -> a + b
mkSum x y = Sum x y
