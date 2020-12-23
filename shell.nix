{ pkgs ? import <nixpkgs> { } }:
with pkgs;

let
  compileHaskell = day: inputFile: pkgs.runCommand "aoc-builder" {} ''
    mkdir -p $out/bin
    ${myGhc}/bin/ghc --make -O3 -rtsopts -threaded -o "$out/bin/d${day}" ${inputFile}
  '';
  myHaskellPackages = ps:
    with ps; [
      array
      adjunctions
      comonad
      fgl
      grid
      lens
      lens
      monad-extras
      monad-loops
      parsec
      pointedlist
      split
      tasty
      tasty-hspec
      text
      unordered-containers
      stm
      stm-linkedlist
    ];
  myGhc = haskellPackages.ghcWithHoogle myHaskellPackages;
  mkDay = n:
    let n' = if n < 10 then "0${toString n}" else toString n;
    in compileHaskell n' (./day + "${n'}.hs");
in mkShell { buildInputs = [ myGhc ] ++ map mkDay (lib.range 1 23); }
