{ nixpkgs ? import ./nix/nixpkgs.nix {} }:

with nixpkgs.pkgs;
with gitignore;

let
  hdl = "verilog";
  hs-build = haskellPackages.callCabal2nix "simple-nix" (gitignoreSource ./.) {};
  in haskell.lib.overrideCabal hs-build (drv: {
    enableLibraryProfiling = false;

    postBuild = ''
      dist/build/clash/clash \
        Example.Project --${hdl} \
        -package-db $TMPDIR/package.conf.d \
        -package-db dist/package.conf.inplace
    '';

    postInstall = ''
      mkdir -p $out/share
      cp -r ${hdl}/Example.Project.topEntity $out/share/${hdl}
    '';
  })
