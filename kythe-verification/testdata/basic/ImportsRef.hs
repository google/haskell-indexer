-- Verify the references to the imported modules
module Imports where

-- - @T ref/imports Data.Text
import Data.Text as T
-- - ref/imports Data.List.groupBy
-- - ref/imports Data.List.sortBy
import Data.List (groupBy, sortBy)

