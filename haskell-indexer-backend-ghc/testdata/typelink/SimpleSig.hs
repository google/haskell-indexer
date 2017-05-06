{-# LANGUAGE RankNTypes #-}
module SimpleSig where

unscoped :: a
unscoped = undefined

scoped :: forall a . a
scoped = undefined
