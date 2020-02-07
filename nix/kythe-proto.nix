{ mkDerivation, base, Cabal, proto-lens-runtime
, proto-lens-setup, stdenv
, indexerSrc }:
mkDerivation {
  pname = "kythe-proto";
  version = "0.1.0.0";
  src = indexerSrc;
  postUnpack = "sourceRoot+=/kythe-proto; echo source root reset to $sourceRoot";
  setupHaskellDepends = [ base Cabal proto-lens-setup ];
  libraryHaskellDepends = [ base proto-lens-runtime ];
  homepage = "https://github.com/google/haskell-indexer";
  description = "Proto bindings for Kythe protobufs";
  license = stdenv.lib.licenses.asl20;
}
