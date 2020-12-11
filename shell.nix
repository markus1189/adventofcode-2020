{ pkgs ? import <nixpkgs> { } }:
with pkgs;

let
  myHaskellPackages = ps:
    with ps; [
      adjunctions
      comonad
      fgl
      grid
      lens
      lens
      parsec
      pointedlist
      split
      tasty
      tasty-hspec
      text
    ];
  myGhc = haskellPackages.ghcWithHoogle myHaskellPackages;
  mkDay = n:
    writeScriptBin "d${toString n}" ''
      ${myGhc}/bin/runhaskell --ghc-arg=-Wall day${toString n}.hs
    '';
in mkShell { buildInputs = [ myGhc ] ++ map mkDay (lib.range 1 24); }
