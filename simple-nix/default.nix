{ nixpkgs ? import ./nix/nixpkgs.nix {} }:

with nixpkgs.pkgs;
with gitignore;

let
  hdl = "verilog";
  topModule = "Example.Project";
  hs-build = haskellPackages.callCabal2nix "simple-nix" (gitignoreSource ./.) {};
in haskell.lib.overrideCabal hs-build (drv: {
  enableLibraryProfiling = false;

  postBuild = ''
    dist/build/clash/clash \
      ${topModule} --${hdl} \
      -package-db dist/package.conf.inplace
  '';

  postInstall = ''
    mkdir -p "$out/share"
    cp -r "${hdl}/" "$out/share/${hdl}"
  '';
})
