-- This file is just for checking that Kythe verifier doesn't fail with
-- duplicate anchors when type variables are used in type signatures.
module TypeVarInSig where

-- The 'a' below causes duplicate anchors, and makes kythe-verifier fail
-- without the `--ignore_dups` flag.
isJust :: Maybe a -> Bool
isJust (Just x) = True
isJust Nothing = False
