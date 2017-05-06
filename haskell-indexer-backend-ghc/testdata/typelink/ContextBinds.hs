{-# LANGUAGE GADTs, RankNTypes #-}
module ContextBinds where

foo :: Eq a => a
foo = undefined

bar :: (Eq a, a ~ b) => b
bar = undefined

baz :: forall a . (Eq a) => a
baz = undefined
