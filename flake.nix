{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs { inherit system; };
      ghc = pkgs.haskellPackages.ghcWithPackages
        (pkgs: with pkgs; [ cabal-install haskell-language-server ]);
    in
    {
      packages.default = pkgs.haskellPackages.developPackage {
        root = ./.;
        modifier = drv:
          pkgs.haskell.lib.addBuildTools drv (with pkgs.haskellPackages;
            [ cabal-install
              base containers optparse-applicative
            ]);
      };

      devShell.default = pkgs.mkShell {
        buildInputs = [ ghc ];
      };
    });
}