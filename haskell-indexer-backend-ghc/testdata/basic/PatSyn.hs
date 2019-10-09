{-# LANGUAGE PatternSynonyms #-}
module PatSyn where

pattern UniSingle a <- [a]

uniAsPattern (UniSingle x) = x

pattern BiSingle a = [a]

biAsExpr x = BiSingle x

biAsPattern (BiSingle x) = x
