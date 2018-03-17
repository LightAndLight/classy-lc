{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc821", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = import ./classy-lc.nix;

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  drv
