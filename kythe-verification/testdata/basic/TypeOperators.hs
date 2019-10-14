{-# LANGUAGE TypeOperators #-}

module TypeOperator where

-- - @"+" defines/binding SumTypeOp
data a + b = Sum a b

-- - @"+" ref SumTypeOp
mkSum :: a -> b -> a + b
mkSum x y = Sum x y
