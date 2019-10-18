module DocUri where

import Data.Maybe (catMaybes)

f :: [Maybe a] -> [a]
f = catMaybes
