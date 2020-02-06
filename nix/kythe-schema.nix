{ mkDerivation, base, bytestring, data-default
, kythe-proto, lens-family, proto-lens, stdenv, text
, indexerSrc }:
mkDerivation {
  pname = "kythe-schema";
  version = "0.1.0.0";
  src = indexerSrc;
  postUnpack = "sourceRoot+=/kythe-schema; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    base bytestring data-default kythe-proto lens-family proto-lens
    text
  ];
  homepage = "https://github.com/google/haskell-indexer";
  description = "Library for emitting Kythe schema entries";
  license = stdenv.lib.licenses.asl20;
}
