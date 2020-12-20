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
      monad-extras
      parsec
      pointedlist
      split
      tasty
      tasty-hspec
      text
    ];
  myGhc = haskellPackages.ghcWithHoogle myHaskellPackages;
  mkDay = n:
    let n' = if n < 10 then "0${toString n}" else toString n;
    in writeScriptBin "d${n'}" ''
      ${myGhc}/bin/runhaskell --ghc-arg=-Wall day${n'}.hs
    '';
in mkShell { buildInputs = [ myGhc ] ++ map mkDay (lib.range 1 24); }
