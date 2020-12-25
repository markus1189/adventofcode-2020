{ pkgs ? import <nixpkgs> { } }:
with pkgs;

let
  compileHaskell = day: inputFile: pkgs.runCommand "aoc-builder" {} ''
    mkdir -p $out/bin
    ${myGhc}/bin/ghc --make -O3 -rtsopts -threaded -o "$out/bin/d${day}-compiled" ${inputFile}
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
    ];
  myGhc = haskellPackages.ghcWithHoogle myHaskellPackages;
  compileDay = n:
    let n' = if n < 10 then "0${toString n}" else toString n;
    in compileHaskell n' (./day + "${n'}.hs");
  mkDay = n:
    let n' = if n < 10 then "0${toString n}" else toString n;
    in writeScriptBin "d${n'}" ''
      ${myGhc}/bin/runhaskell --ghc-arg=-Wall day${n'}.hs
    '';
  range = lib.range 1 25;
in mkShell { buildInputs = [ myGhc ] ++ map mkDay range ++ map compileDay range; }
