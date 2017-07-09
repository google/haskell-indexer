{-# LANGUAGE GADTs, RankNTypes #-}
module ContextBinds where

foo :: Eq c => c
foo = undefined

bar :: (Eq a, a ~ b) => b
bar = undefined

baz :: forall a . (Eq a) => a
baz = undefined
