{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  haskellPackages = if compiler == "default"
                      then pkgs.haskellPackages
                      else pkgs.haskell.packages.${compiler};

in

  with haskellPackages;
  shellFor {
    packages = p: [ (callPackage ./. {}) ];
    buildInputs = [ cabal-install ];
  }
