{ pkgs ? import <nixpkgs> { } }:
with pkgs;

let
  myHaskellPackages = ps: with ps; [ lens ];
  myGhc = haskellPackages.ghcWithHoogle myHaskellPackages;
  mkDay = n:
    writeScriptBin "d${toString n}" ''
      ${myGhc}/bin/runhaskell --ghc-arg=-Wall --ghc-arg=-Werror day${
        toString n
      }.hs
    '';
  days = [ 1 ];
in mkShell { buildInputs = [ myGhc ] ++ map mkDay days; }
