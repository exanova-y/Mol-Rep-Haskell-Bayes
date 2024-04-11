{ pkgs ? import <nixpkgs> {} }:
let
  ghc = pkgs.haskell.packages.ghc92.ghcWithPackages
          (p: [ p.random
                p.monad-extras
                p.log-domain
                p.statistics
              ]);
in
pkgs.mkShell {
  buildInputs = [ ghc pkgs.cabal-install pkgs.ripgrep ];
}
