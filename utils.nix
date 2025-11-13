{ pkgs, lib, ... }: rec {
  # Generate an attribute set by applying a function to forEach string of a list
  # forEachString
  #   :: [String]
  #   -> (String -> a)
  #   -> { String :: a }
  forEachString = xs: f:
    builtins.listToAttrs (builtins.map (x: lib.nameValuePair x (f x)) xs);

  # Generate an attribute set for forEach package in a project
  # forEachPackage
  #   :: Project
  #   -> (String -> a)
  #   -> { String :: a }
  forEachPackage = project: f:
    forEachString (builtins.attrNames project.packages) (pkg: f pkg);

  # Generate an attribute set for forEach script a project
  # forEachScript
  #   :: Project
  #   -> (String -> a)
  #   -> { String :: a }
  forEachScript = project: f:
    forEachString (builtins.attrNames project.scripts) (script: f script);

  # Generate an attribute set for forEach supported GHC version in a project
  # forEachSupportedGHCVersion
  #   :: Project
  #   -> (String -> HaskellPackages -> a)
  #   -> { String :: a }
  forEachSupportedGHCVersion = project: f:
    forEachString project.supportedGHCVersions (ghcVersion: f ghcVersion);

  # Create a Haskell packages for a given compiler with project overrides
  # haskellPackagesFor
  #   :: Project
  #   -> { String :: a }
  #   -> { String :: a }
  #   -> HaskellPackages
  haskellWithProjectPackagesFor = project: compiler: overrides:
    pkgs.haskell.packages.${compiler}.override {
      overrides = self: super:
        forEachPackage project (pkg:
          (mkPackage self ({ name = pkg; } // project.packages.${pkg}
            // overrides)));
    };

  # Create a Haskell package with optional overrides and cabal2nix options
  # mkPackage
  #   :: HaskellPackages
  #   -> { name: String,
  #      , src: Path,
  #      , flags : [ String ] ? []
  #      , doCheck : Bool ? false
  #      , doHaddock : Bool ? false
  #      , doBench : Bool ? false
  #      , overrides: { String :: Path } ? {}
  #      }
  #   -> Derivation
  mkPackage = hsPkgs:
    { name, src, flags ? [ ], doCheck ? false, doHaddock ? false
    , doBench ? false, overrides ? { } }:
    let
      # Build cabal2nix options based on flags
      opts = lib.concatStringsSep " " [
        (if doCheck then "" else "--no-check")
        (if doHaddock then "" else "--no-haddock")
        (if doBench then "--benchmark" else "")
        (lib.concatStringsSep " " (builtins.map (flag: "--flag=" + flag) flags))
      ];
      # Build dependency overrides without any extra options
      overrideDrvs = forEachString (builtins.attrNames overrides) (dep:
        hsPkgs.callCabal2nixWithOptions dep overrides.${dep}
        "--no-check --no-haddock" { });
      # Create the derivation for the package itself
      drv = hsPkgs.callCabal2nixWithOptions name src opts overrideDrvs;

    in if doHaddock
    # If documentation is built, create a symlinked package including docs.
    # Otherwise, just return the derivation as is.
    then
      pkgs.symlinkJoin {
        name = drv.name;
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
  #   -> { packages: { String :: a }; devShells: { String :: a }; }
  mkFlake = system: project: rec {
    packages.${system} = {
      # packages.default
      default = packages.${system}.${project.defaultGHC}.mutagen;
      # packages.scripts.<script>
      scripts = forEachScript project (mkScript project);
    } //
      # packages.<ghc_version>.<package>
      forEachSupportedGHCVersion project (ghcVersion:
        let hsPkgs = haskellWithProjectPackagesFor project ghcVersion { };
        in forEachPackage project (pkg: hsPkgs.${pkg})) //
      # packages.ci.<ghc_version>.<package>
      {
        ci = forEachSupportedGHCVersion project (ghcVersion:
          let
            hsPkgs = haskellWithProjectPackagesFor project ghcVersion {
              doCheck = true;
              doHaddock = true;
              doBench = true;
            };
          in forEachPackage project (pkg: hsPkgs.${pkg}));
      };

    devShells.${system} = {
      # devShells.default
      default = devShells.${system}.${project.defaultGHC};
    } //
      # devShells.<ghc_version>
      forEachSupportedGHCVersion project (ghcVersion:
        let hsPkgs = haskellWithProjectPackagesFor project ghcVersion { };
        in hsPkgs.shellFor {
          packages = ps:
            builtins.map (pkg: ps.${pkg}) (builtins.attrNames project.packages);
          withHoogle = true;
          buildInputs = [ hsPkgs.cabal-install hsPkgs.haskell-language-server ]
            ++ (builtins.attrValues (forEachScript project (mkScript project)));
        });
  };

}
