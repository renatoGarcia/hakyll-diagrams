{ pkgs ? import <nixpkgs> { } }:

let
  haskellPackages = pkgs.haskellPackages;

  drv = haskellPackages.developPackage {
    root = ./.;
    modifier = drv:
      pkgs.haskell.lib.addBuildTools drv (with haskellPackages; [
        cabal-install
        haskell-language-server
        hlint
        fourmolu
      ]);
  };
in
drv.env
