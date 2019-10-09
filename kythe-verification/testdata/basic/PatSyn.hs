{-# LANGUAGE PatternSynonyms #-}
module PatSyn where

-- - @UniSingle defines/binding PatUniSingle
pattern UniSingle a <- [a]

-- - @UniSingle ref PatUniSingle
uniAsPattern (UniSingle x) = x

-- - @BiSingle defines/binding PatBiSingle
pattern BiSingle a = [a]

-- - @BiSingle ref PatBiSingle
biAsExpr x = BiSingle x

-- - @BiSingle ref PatBiSingle
biAsPattern (BiSingle x) = x
