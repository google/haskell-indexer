{ mkDerivation, base, bytestring, ghc
, haskell-indexer-backend-core, haskell-indexer-backend-ghc
, haskell-indexer-pathutil, haskell-indexer-pipeline-ghckythe
, haskell-indexer-translate, kythe-schema, optparse-applicative
, proto-lens, stdenv, text
, indexerSrc }:
mkDerivation {
  pname = "haskell-indexer-pipeline-ghckythe-wrapper";
  version = "0.1.0.0";
  src = indexerSrc;
  postUnpack = "sourceRoot+=/haskell-indexer-pipeline-ghckythe-wrapper; echo source root reset to $sourceRoot";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring ghc haskell-indexer-backend-core
    haskell-indexer-backend-ghc haskell-indexer-pathutil
    haskell-indexer-pipeline-ghckythe haskell-indexer-translate
    kythe-schema optparse-applicative proto-lens text
  ];
  executableHaskellDepends = [ base ];
  homepage = "https://github.com/google/haskell-indexer";
  description = "Executable able to take GHC arguments and emitting Kythe entries";
  license = stdenv.lib.licenses.asl20;
}
