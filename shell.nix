let
  nixpkgs = import <nixpkgs> { overlays = [ nixpkgsOverlay ]; };

  nixpkgsOverlay = self: _: {
    devPkgs =
      self.haskellPackages.override (_: { overrides = haskellOverrides; });
  };

  # Create a Haskell package for this project using its cabal file
  haskellOverrides = self: _: {
    mutagen = self.callCabal2nix "mutagen" ./. { };
  };

  # Development tools
  devTools = with nixpkgs; [
    ghc # Compiler
    cabal-install
    haskell-language-server
  ];

in nixpkgs.devPkgs.mutagen.env.overrideAttrs
(env: { nativeBuildInputs = env.nativeBuildInputs ++ devTools; })
