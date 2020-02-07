{ mkDerivation, base, filepath, stdenv, text
, indexerSrc}:
mkDerivation {
  pname = "haskell-indexer-pathutil";
  version = "0.1.0.0";
  src = indexerSrc;
  postUnpack = "sourceRoot+=/haskell-indexer-pathutil; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [ base filepath text ];
  homepage = "https://github.com/google/haskell-indexer";
  description = "Utilities for dealing with filepaths";
  license = stdenv.lib.licenses.asl20;
}
