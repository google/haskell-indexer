module CrossRef2 where

-- - @"CrossRef1" ref/imports vname("main_main:CrossRef1", "", "", "", "haskell")
import CrossRef1 (foo)

bar :: Int
-- - @foo ref _
bar = foo + 1
