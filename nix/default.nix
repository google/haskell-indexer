{ pkgs ? import <nixpkgs> {}
}:

with {
  haskellPackages = pkgs.haskellPackages.override (old: {
    overrides =
      with pkgs;
      with {
        indexerSrc = fetchgit {
          url = "https://github.com/google/haskell-indexer";
          sha256 = "0rr32birxd5xw85jyjx2q3dfgv7szpd142hbwf6isgiv3a3bhkx8";
          rev = "08aed342bc76315534e7ee06ed497689276279e6";
        };
      };
      lib.composeExtensions (old.overrides or (_: _: {})) (self: super: rec {
        kythe-proto = haskell.lib.addBuildDepend (super.callPackage ./kythe-proto.nix { inherit indexerSrc; }) [protobuf];
        kythe-schema = super.callPackage ./kythe-schema.nix { inherit indexerSrc; };
        text-offset = super.callPackage ./text-offset.nix { inherit indexerSrc; };
        haskell-indexer-backend-core = super.callPackage ./haskell-indexer-backend-core.nix { inherit indexerSrc; };
        haskell-indexer-backend-ghc = super.callPackage ./haskell-indexer-backend-ghc.nix { inherit indexerSrc; };
        haskell-indexer-frontend-kythe = super.callPackage ./haskell-indexer-frontend-kythe.nix { inherit indexerSrc; };
        haskell-indexer-pathutil = super.callPackage ./haskell-indexer-pathutil.nix { inherit indexerSrc; };
        haskell-indexer-pipeline-ghckythe = super.callPackage ./haskell-indexer-pipeline-ghckythe.nix { inherit indexerSrc; };
        haskell-indexer-pipeline-ghckythe-wrapper = super.callPackage ./haskell-indexer-pipeline-ghckythe-wrapper.nix { inherit indexerSrc; };
        haskell-indexer-translate = super.callPackage ./haskell-indexer-translate.nix { inherit indexerSrc; };
      });
  });
};
{
  indexer = haskellPackages.haskell-indexer-pipeline-ghckythe-wrapper;
}
