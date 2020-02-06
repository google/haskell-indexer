{ mkDerivation, base, bytestring, conduit, filepath
, haskell-indexer-backend-core, haskell-indexer-backend-ghc
, haskell-indexer-frontend-kythe, haskell-indexer-translate
, kythe-schema, mmorph, mtl, stdenv, text
, indexerSrc }:
mkDerivation {
  pname = "haskell-indexer-pipeline-ghckythe";
  version = "0.1.0.0";
  src = indexerSrc;
  postUnpack = "sourceRoot+=/haskell-indexer-pipeline-ghckythe; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    base bytestring conduit filepath haskell-indexer-backend-core
    haskell-indexer-backend-ghc haskell-indexer-frontend-kythe
    haskell-indexer-translate kythe-schema mmorph mtl text
  ];
  homepage = "https://github.com/google/haskell-indexer";
  description = "Gets GHC invocation arguments and streams Kythe entries";
  license = stdenv.lib.licenses.asl20;
}
