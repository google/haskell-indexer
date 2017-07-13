-- Verify the references to the imported modules
module Imports where

-- - @"Data.Set" ref/imports vname("containers-0.5.7.1:Data.Set", "", "", "", "haskell")
import Data.Set as S
-- - @"Data.List" ref/imports vname("base:Data.List", "", "", "", "haskell")
import Data.List (groupBy, sortBy)
