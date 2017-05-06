{-# LANGUAGE ScopedTypeVariables #-}
module InlineSig where

foo :: forall a . a
foo = undefined :: a
