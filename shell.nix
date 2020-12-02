{ pkgs ? import <nixpkgs> { } }:
with pkgs;

let
  myHaskellPackages = ps: with ps; [ lens text parsec ];
  myGhc = haskellPackages.ghcWithHoogle myHaskellPackages;
  mkDay = n:
    writeScriptBin "d${toString n}" ''
      ${myGhc}/bin/runhaskell --ghc-arg=-Wall day${toString n}.hs
    '';
  days = [ 1 2 ];
in mkShell { buildInputs = [ myGhc ] ++ map mkDay days; }
