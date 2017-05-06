{-# LANGUAGE RankNTypes #-}
module ForallBinds where

implicit :: a -> a
implicit = undefined

explicit :: forall a . a -> a
explicit = undefined
