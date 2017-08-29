module CrossRef2 where

-- - @"CrossRef1" ref/imports Module
import CrossRef1 (foo)
-- - @"Data.List" ref/imports vname("base:Data.List", "", "", "", "haskell")
import Data.List (groupBy, sortBy)

bar :: Int
-- - @foo ref Fun
bar = foo + 1
