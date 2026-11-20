<!-- omit in toc -->
# Simple Nix Starter Project
This starter project contains the scaffolding needed to integrate Clash with the Nix build system. Read [Simple Starter Project](https://github.com/clash-lang/clash-starters/blob/main/simple/README.md) for more information on the various files.

<!-- omit in toc -->
# Table of Contents
- [Getting this project](#getting-this-project)
- [Building and testing this project](#building-and-testing-this-project)
- [REPL](#repl)
- [Adding custom dependencies / updating nix](#adding-custom-dependencies--updating-nix)

# Getting this project
First create a new directory and enter it: `mkdir starter && cd starter`

Then run: `nix flake init -t github:clash-lang/clash-starters`.

# Building and testing this project
Build the project with:

```bash
nix run
```

Verilog code will be available under the `verilog` directory.
Modify the `hdl` variable in `flake.nix` to configure whether to generate
SystemVerilog or VHDL.

However development itself is more streamlined by using a Nix shell. Start one 
by invoking:

```
nix develop
```

Then, to run the tests defined in `tests/`:

```bash
cabal run test-library
cabal run doctests
```

To compile the project to VHDL run:

```bash
cabal run clash -- Example.Project --vhdl
```

You can find the HDL files in `vhdl/`. The source can be found in `src/Example/Project.hs`.

# VSCode

In order to make VSCode use the right version of HLS (the Haskell language server), you will need to
install [this](https://marketplace.visualstudio.com/items?itemName=mkhl.direnv) plugin.

Without it, VSCode will attempt to use the wrong version of HLS and (mostly) crash.

# REPL
Clash offers a [REPL](https://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop) as a quick way to try things, similar to Python's `python` or Ruby's `irb`. Open the REPL by invoking:

```
cabal run clashi
```

# Adding custom dependencies / updating nix
The nix flake contains the sources of all Haskell and non-Haskell related packages.
You can add newer/older packages by overwriting them using the `overlay` defined in `flake.nix`, or
if the package is not available, include it by adding it as a flake input.

Example:

```diff
package-overlay = final: prev: {
  # Here you define the project you want to build
  simple-nix = prev.developPackage {
    root = ./.;
    overrides = _: _: final;
  };
+ # Adds an older version of `hello` package
+ hello = prev.callHackageDirect {
+   pkg = "hello";
+   ver = "1.0";
+   sha256 = "sha256-/oxATGk025R39CkjfWAcX2SrcM+pLY7IqlkPLJYVIIc=";
+ } {};
};
```

For adding other Clash dependencies which also have a flake (such as clash-protocols), you can use
the overlay those projects expose and append them to your own as follows:

```diff
package-overlay = final: prev: {
  # Here you define the project you want to build
  simple-nix = prev.developPackage {
    root = ./.;
    overrides = _: _: final;
  };
-};
+} // clash-protocols.overlays.${system}.${ghc-version} final prev;
```

When you include those projects as a flake input, make sure to make their `clash-compiler` input
follow your own! Aka; set `clash-protocols.inputs.clash-compiler.follows = "clash-compiler";`
