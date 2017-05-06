{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
module RecordRead where

data Rec = Rec { field :: Int, baar :: Int }

unpack (Rec f b) = f

access r = field r

wildcard Rec{..} = field

punned Rec{field} = field

reassigned Rec{field=bla} = bla

data Complex = Complex { comp :: (Int,Int) }
complexMatched Complex{comp=(a,b)} = a

rightWild Rec{baar,..} = field + baar
