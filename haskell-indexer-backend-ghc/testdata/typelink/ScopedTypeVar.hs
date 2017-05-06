{-# LANGUAGE ScopedTypeVariables #-}
module ScopedTypeVar where

unscoped :: a -> a
unscoped = bar
  where
    bar :: a -> a
    bar = id

scoped :: forall a . a -> a
scoped = bars
  where
    bars :: a -> a
    bars = id
