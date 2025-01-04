{ sources ? import ./sources.nix }:

let
  overlay = _: pkgs: {

    # Nix tooling
    niv = (import sources.niv {}).niv;
    gitignore = import sources.gitignore { inherit (pkgs) lib; };

    # Haskell overrides
    haskellPackages = pkgs.haskellPackages.override {
      overrides = self: super: {
        # Add overrides here
        doctest-parallel =
          self.callCabal2nix "doctest-parallel" sources.doctest-parallel {};
        clash-prelude =
          self.callCabal2nix "clash-prelude" (sources.clash-compiler + "/clash-prelude") {};
        clash-lib =
          self.callCabal2nix "clash-lib" (sources.clash-compiler + "/clash-lib") {};
        clash-ghc =
          self.callCabal2nix "clash-ghc" (sources.clash-compiler + "/clash-ghc") {};
        clash-prelude-hedgehog =
          self.callCabal2nix "clash-prelude" (sources.clash-compiler + "/clash-prelude-hedgehog") {};
        tasty-hedgehog =
          self.callCabal2nix "tasty-hedgehog" sources.tasty-hedgehog {};
        hedgehog =
          self.callCabal2nix "hedgehog" (sources.haskell-hedgehog + "/hedgehog") {};
      };
    };
  };

in import sources.nixpkgs { overlays = [ overlay ]; }
