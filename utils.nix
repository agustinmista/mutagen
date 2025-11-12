{ pkgs, lib, ... }: rec {
  # Generate an attribute set by applying a function to each string of a list
  # eachString
  #   :: [String]
  #   -> (String -> a)
  #   -> { String :: a }
  eachString = xs: f:
    builtins.listToAttrs (builtins.map (x: lib.nameValuePair x (f x)) xs);

  # Generate an attribute set for each supported GHC version in a project
  # eachSupportedGHCVersion
  #   :: Project
  #   -> (String -> HaskellPackages -> a)
  #   -> { String :: a }
  eachSupportedGHCVersion = project: f:
    eachString project.supportedGHCVersions (ghcVersion:
      f ghcVersion (haskellWithProjectPackagesFor project ghcVersion));

  # Generate an attribute set for each package in a project
  # eachPackage
  #   :: Project
  #   -> (String -> a)
  #   -> { String :: a }
  eachPackage = project: f:
    eachString (builtins.attrNames project.packages) (pkg: f pkg);

  # Generate an attribute set for each script a project
  # eachScript
  #   :: AttrSet
  #   -> (String -> a)
  #   -> { String :: a }
  eachScript = project: f:
    eachString (builtins.attrNames project.scripts) (script: f script);

  # Create a Haskell packages for a given compiler with project overrides
  # haskellPackagesFor
  #   :: Project
  #   -> String
  #   -> HaskellPackages
  haskellWithProjectPackagesFor = project: compiler:
    pkgs.haskell.packages.${compiler}.override {
      overrides = self: super:
        eachPackage project
        (pkg: (mkPackage self ({ name = pkg; } // project.packages.${pkg})));
    };

  # Create a Haskell package with optional overrides and cabal2nix options
  # mkPackage
  #   :: HaskellPackages
  #   -> { name: String,
  #      , src: Path,
  #      , overrides: { String :: Path } ? {}
  #      , doCheck : Bool ? true
  #      , doHaddock : Bool ? true
  #      }
  #   -> Derivation
  mkPackage = hsPkgs:
    { name, src, overrides ? { }, doCheck ? true, doHaddock ? true }:
    let
      # Build cabal2nix options based on flags
      opts = lib.concatStringsSep " " [
        (if doCheck then "" else "--no-check")
        (if doHaddock then "" else "--no-haddock")
      ];
      # Build dependency overrides without any extra options
      overrideDrvs = eachString (builtins.attrNames overrides) (dep:
        hsPkgs.callCabal2nixWithOptions dep overrides.${dep}
        "--no-check --no-haddock" { });
      drv = hsPkgs.callCabal2nixWithOptions name src opts overrideDrvs;

    in if doHaddock
    # If documentation is built, create a symlinked package including docs.
    # Otherwise, just return the derivation as is.
    then
      pkgs.symlinkJoin {
        name = drv.pname + "-with-docs";
        paths = [ drv.out drv.doc ];
        passthru = drv.out.passthru or { }; # needed by 'shellFor'
      }
    else
      drv;

  # Create a shell application for a given script
  # mkScript
  #   :: Project
  #   -> String
  #   -> Derivation
  mkScript = project: script:
    pkgs.writeShellApplication
    ({ name = script; } // project.scripts.${script});

  # Create the packages and devShells attribute sets for a flake
  # mkFlake
  #   :: System
  #   -> Project
  #   -> { packages: AttrSet; devShells: AttrSet; }
  mkFlake = system: project: rec {
    packages.${system} = {
      # packages.default
      default = packages.${system}.${project.defaultGHC}.mutagen;
      # packages.scripts.<script>
      scripts = eachScript project (mkScript project);
    } //
      # packages.<ghc_version>.<package>
      eachSupportedGHCVersion project
      (ghcVersion: hsPkgs: eachPackage project (pkg: hsPkgs.${pkg}));
    devShells.${system} = {
      # devShells.default
      default = devShells.${system}.${project.defaultGHC};
    } //
      # devShells.<ghc_version>
      eachSupportedGHCVersion project (ghcVersion: hsPkgs:
        hsPkgs.shellFor {
          packages = ps:
            builtins.map (pkg: ps.${pkg}) (builtins.attrNames project.packages);
          withHoogle = true;
          buildInputs = [ hsPkgs.cabal-install hsPkgs.haskell-language-server ]
            ++ (builtins.attrValues (eachScript project (mkScript project)));
        });
  };

}
