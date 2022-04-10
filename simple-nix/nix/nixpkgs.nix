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
          pkgs.haskell.lib.dontCheck (self.callCabal2nix "doctest-parallel" sources.doctest-parallel {});
        clash-prelude =
          pkgs.haskell.lib.dontCheck (self.callCabal2nix "clash-prelude" sources.clash-prelude {});
        clash-lib =
          pkgs.haskell.lib.dontCheck (self.callCabal2nix "clash-lib" sources.clash-lib {});
        clash-ghc =
          pkgs.haskell.lib.dontCheck (self.callCabal2nix "clash-ghc" sources.clash-ghc {});
      };
    };
  };

in import sources.nixpkgs { overlays = [ overlay ]; }
