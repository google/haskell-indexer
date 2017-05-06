module RecursiveRef where
{-# ANN module "HLint: ignore Eta reduce" #-}

-- Recursive function call without type signature targets the monomorphic
-- binding. This verifies that we handle the case.
-- - @recNoSig defines/binding FunRNS
recNoSig x =
    -- - @recNoSig ref FunRNS
    recNoSig x

-- - @localRecNoSig ref FunLRNS
dummy = localRecNoSig
  where
    -- - @localRecNoSig defines/binding FunLRNS
    localRecNoSig x =
        -- - @localRecNoSig ref FunLRNS
        localRecNoSig x

-- Recursive call to function with type signature targets the polymorphic
-- binding.
recWithSig :: Int -> Int
-- - @recWithSig defines/binding FunRWS
recWithSig x =
    -- - @recWithSig ref FunRWS
    recWithSig x

-- - @mutualNoSigA defines/binding FunMA
-- - @mutualNoSigB ref FunMB
mutualNoSigA = mutualNoSigB
-- - @mutualNoSigB defines/binding FunMB
-- - @mutualNoSigA ref FunMA
mutualNoSigB = mutualNoSigA

-- - @etaNoSig defines/binding FunENS
etaNoSig =
    -- - @etaNoSig ref FunENS
    etaNoSig
