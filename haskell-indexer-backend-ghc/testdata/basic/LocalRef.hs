{-# LANGUAGE BangPatterns #-}
module LocalRef where
import Prelude hiding (lookup)

--
difference a b = foldlWithKey' go
  where
    go = case "oaeu" of
                 "oeu" -> "oqeju"
                 _       -> "oeuoeu"
{-# INLINABLE difference #-}

--
intersection a b = foldlWithKey' go (5 :: Int) 3 1
  where
    go x y z = x + y + z
{-# INLINABLE intersection #-}

foldlWithKey' = id
