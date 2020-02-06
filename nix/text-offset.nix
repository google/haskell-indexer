{ mkDerivation, base, bytestring, HUnit, QuickCheck
, stdenv, test-framework, test-framework-hunit
, test-framework-quickcheck2, text, vector
, indexerSrc }:
mkDerivation {
  pname = "text-offset";
  version = "0.1.0.0";
  src = indexerSrc;
  postUnpack = "sourceRoot+=/text-offset; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [ base text vector ];
  testHaskellDepends = [
    base bytestring HUnit QuickCheck test-framework
    test-framework-hunit test-framework-quickcheck2 text
  ];
  homepage = "https://github.com/google/haskell-indexer";
  description = "Library for converting between line/column and byte offset";
  license = stdenv.lib.licenses.asl20;
}
