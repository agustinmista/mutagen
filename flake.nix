{
  description = "MUTAGEN";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  };

  outputs =
    {
      nixpkgs,
      flake-utils,
      ...
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs { inherit system; };

        hsPkgs = pkgs.haskell.packages.ghc98.override {
          overrides = self: super: {
            mutagen = self.callCabal2nix "mutagen" ./. {
              # Dependency overrides can go here
            };
          };
        };
      in
      {
        packages = {
          default = hsPkgs.mutagen;
        };

        devShells = rec {
          default = ghc98;

          ghc98 = hsPkgs.shellFor {
            packages = ps: [ ps.mutagen ];
            withHoogle = true;
            buildInputs = [
              hsPkgs.cabal-install
              hsPkgs.haskell-language-server
            ];
          };
        };
      }
    );
}
