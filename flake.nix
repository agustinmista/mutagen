{
  description = "MUTAGEN";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      ...
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs { inherit system; };
        lib = pkgs.lib;

        defaultGHC = "ghc98";

        supportedGHCs = [
          "ghc96"
          "ghc98"
          "ghc910" # has some TH issues
          "ghc912" # has some TH issues
        ];

        hsPkgsFor =
          compiler:
          pkgs.haskell.packages.${compiler}.override {
            overrides = self: super: {
              mutagen = self.callCabal2nix "mutagen" ./. {
                # Pin dependencies that are not yet in nixpkgs
                pqueue = self.callCabal2nix "pqueue" (pkgs.fetchFromGitHub {
                  owner = "lspitzner";
                  repo = "pqueue";
                  rev = "91f1a36271e16ddc4c4777c66a6725e751811068"; # 1.6
                  sha256 = "sha256-e0a5ULT2HPgkcWLFTTpBfgbbhoBRLMsMLh52knJy2HE=";
                }) { };
                th-desugar = self.callCabal2nix "th-desugar" (pkgs.fetchFromGitHub {
                  owner = "goldfirere";
                  repo = "th-desugar";
                  rev = "1c9e884f80d6e8efb5109cb85bb889a12bc9ed8d"; # 1.19
                  sha256 = "sha256-Yweb7966h9hSN4ALNncyfS5QfAoSDBm4AozXqMQ4+3Q=";
                }) { };
              };
            };
          };

        forEachSupportedGHC =
          f:
          builtins.listToAttrs (
            builtins.map (compiler: lib.nameValuePair compiler (f (hsPkgsFor compiler))) supportedGHCs
          );
      in
      {
        packages = {
          default = self.packages.${system}.${defaultGHC}.mutagen;
        }
        // forEachSupportedGHC (hsPkgs: {
          inherit (hsPkgs) mutagen;
        });

        devShells = {
          default = self.devShells.${system}.${defaultGHC};
        }
        // forEachSupportedGHC (
          hsPkgs:
          hsPkgs.shellFor {
            packages = ps: [ ps.mutagen ];
            withHoogle = true;
            buildInputs = [
              hsPkgs.cabal-install
              hsPkgs.haskell-language-server
            ];
          }
        );
      }
    );
}
