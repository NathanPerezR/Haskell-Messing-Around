{
  description = "Haskell development environment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";

      pkgs = import nixpkgs {
        inherit system;
      };

      hpkgs = pkgs.haskell.packages.ghc98;
    in {
      devShells.${system}.default = pkgs.mkShell {
        packages = [
          hpkgs.ghc
          hpkgs.cabal-install
          hpkgs.haskell-language-server

          pkgs.ormolu
          pkgs.hlint
        ];

        shellHook = ''
          echo "Haskell dev shell loaded"
          ghc --version
        '';
      };
    };
}
