{
  description = "A flake for starter Clash projects!";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    clash-compiler.url = "github:clash-lang/clash-compiler?ref=v1.8.4";
  };
  nixConfig = {
    extra-substituters = [ "https://clash-lang.cachix.org" ];
    extra-trusted-substituters = [ "https://clash-lang.cachix.org" ];
    extra-trusted-public-keys = [ "clash-lang.cachix.org-1:/2N1uka38B/heaOAC+Ztd/EWLmF0RLfizWgC5tamCBg=" ];
  };
  outputs = { self, nixpkgs, flake-utils, clash-compiler }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        # A list of regular packages you want to use with your project
        pkgs = (import nixpkgs {
          inherit system;
        });

        # What version of GHC you want to use
        # The version must be in the list of supported versions of the clash-compiler!
        ghc-version = "ghc9101";

        # A list of HASKELL packages clash-compiler uses, we use this set packages to prevent version
        # conflicts with the regular nixpkgs when using Clash
        clash-pkgs = ((import clash-compiler.inputs.nixpkgs {
          inherit system;
        }).extend clash-compiler.overlays.${ghc-version})."clashPackages-${ghc-version}";

        package-overlay = final: prev: {
          # Here you define the project you want to build
          simple-nix = prev.developPackage {
            root = ./.;
            overrides = _: _: final;
          };
        };

        # Create a set of Haskell packages including ours!
        hs-pkgs = clash-pkgs.extend package-overlay;

        # Options for `nix run`
        # Select the toplevel module
        top-module = "Example.Project";
        # Output VHDL or Verilog
        hdl = "verilog";
      in
        {
          # Develop the project using `nix develop`
          devShells.default = hs-pkgs.shellFor {
            # Under `packages` we list out all Haskell packages we want to develop *for*
            # This will then grab all the dependencies for those packages
            # Note that this does *not* build the package itself! Only the dependencies of the package
            packages = p: [
              p.simple-nix
            ];

            nativeBuildInputs = [
              # Standard
              hs-pkgs.cabal-install
              hs-pkgs.cabal-plan
              hs-pkgs.fourmolu
              hs-pkgs.haskell-language-server

              # Regular dependencies you may want to use in your project
              pkgs.hello
            ];
          };

          # Build the project with Clash and generate HDL
          # This can be ran using `nix run` and will output Verilog files under the `verilog` subdirectory
          # The top-module and hdl are variables defined above in the let statement
          apps.default = {
            type = "app";
            program = (pkgs.writeShellScript "compile" ''
              if [ -z "$IN_NIX_SHELL" ]; then
                echo "Run me from within a Nix developer shell!"
                echo "Simply run: nix develop"
                exit 1
              fi
              
              cabal build --write-ghc-environment-files=never
              cabal run clash ${top-module} -- --${hdl}
              echo "Removing" .ghc.environment.*
              rm -f .ghc.environment.*
            '').outPath;
          };

          # Build the project using `nix build`
          packages.default = hs-pkgs.simple-nix;
        }
    );
}
