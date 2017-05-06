module RecursiveRef where
{-# ANN module "HLint: ignore Eta reduce" #-}

recNoSig x = recNoSig x

dummy = localRecNoSig
  where
    localRecNoSig x = localRecNoSig x

recWithSig :: Int -> Int
recWithSig x = recWithSig x

mutualNoSigA = mutualNoSigB
mutualNoSigB = mutualNoSigA

etaNoSig = etaNoSig
