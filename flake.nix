{
  description = "A flake for clash-starter templates. For an actual flake containing the project, check the `simple-nix` subdirectory.";

  outputs = { self }: {
    defaultTemplate = {
      path = ./simple-nix;
      description = "A template containing a simple Clash project with a Nix flake for development";
      welcomeText = ''
        # Nix development
        To use Nix to develop your Clash project, simply run `nix develop` in your current directory.
        This will put you into an environment with all the tools you will need to build the starter
        project.

        To compile the project and generate HDL, simply run `nix run` and the output Verilog will be
        under the `verilog` directory. If you changed the HDL language to VHDL, it will be under the
        `vhdl` directory.

        You can also run `nix build` to build the project using Nix. Although this isn't very
        useful in the starter project, it is very useful when developing libraries!

        # Cabal & Nix clashes
        Cabal and Nix work happily together, most of the time. One important thing of note is that
        you should **not** define sources using `cabal.project` files. This will overwrite Nix's
        package source and *will* cause problems. To add dependencies, add them via `flake.nix`.

        # TLDR
        1. run `nix develop` to immediately jump into development
        2. run `nix run` to build the package and generate HDL with Nix
        3. run `nix build` to build the package as a library with Nix

        Read the README.md for more information!
        Happy hacking!
      '';
    };
  };
}
