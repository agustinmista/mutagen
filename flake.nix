{
  description = "MUTAGEN";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

    # Unpublished packages
    pqueue.url =
      "github:lspitzner/pqueue/91f1a36271e16ddc4c4777c66a6725e751811068"; # 1.6
    pqueue.flake = false;

    th-desugar.url =
      "github:goldfirere/th-desugar/1c9e884f80d6e8efb5109cb85bb889a12bc9ed8d"; # 1.19
    th-desugar.flake = false;
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystemPassThrough (system:
      let
        # Package set for the given system
        pkgs = import nixpkgs { inherit system; };
        # Import utility functions
        util = pkgs.callPackage ./utils.nix { };
        # Define the project
        project = {
          # Default GHC version
          defaultGHC = "ghc910";
          # Supported GHC versions
          supportedGHCVersions = [ "ghc96" "ghc98" "ghc910" "ghc912" ];
          # Local packages to build
          packages.mutagen = {
            src = ./.;
            overrides = { inherit (self.inputs) pqueue th-desugar; };
          };
          # Helper scripts to be included in all dev shells
          scripts.run-fourmolu = {
            runtimeInputs = [ pkgs.fourmolu ];
            text = ''
              MODE=''${MODE:-inplace}
              echo "Running fourmolu in $MODE mode"
              git ls-files '*.hs' | \
                xargs fourmolu \
                  --color always \
                  --check-idempotence \
                  --mode "$MODE"
            '';
          };
        };

      in util.mkFlake system project);
}
