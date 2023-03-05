{ pkgs ? import <nixpkgs> {} }:

let
  ghc = pkgs.haskellPackages.ghcWithPackages
    (pkgs: with pkgs; [ cabal-install haskell-language-server ]);
in
  pkgs.mkShell {
    buildInputs = [ ghc ];

    shellHook = ''
    '';
  }