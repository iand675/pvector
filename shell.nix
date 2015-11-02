{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, deepseq, ghc-prim, mtl, primitive
      , stdenv, criterion
      }:
      mkDerivation {
        pname = "pvector";
        version = "0.1.0.0";
        src = ./.;
        libraryHaskellDepends = [ base deepseq ghc-prim mtl primitive ];
        homepage = "https://github.com/iand675/pvector";
        description = "Efficient persistent vectors and transient vectors";
        license = stdenv.lib.licenses.mit;
      };

  haskellPackages = if compiler == "default"
                      then pkgs.haskellPackages
                      else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
