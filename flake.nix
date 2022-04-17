{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs { inherit system; };

        ghc = pkgs.haskell.packages.ghc8107.ghcWithPackages (
          ps: [
            ps.optparse-applicative
            ps.sdl2
            ps.typelits-witnesses
            ps.vector-sized
          ]
        );

        cabalWrapped = pkgs.writeShellScriptBin "cabal" ''
          ${pkgs.hpack}/bin/hpack && exec ${pkgs.cabal-install}/bin/cabal "$@"
        '';
      in
      {
        defaultPackage = pkgs.haskellPackages.callCabal2nix "haskell-chip8" ./. { };
        devShell = pkgs.mkShell {
          buildInputs = [
            pkgs.nixpkgs-fmt
            pkgs.ormolu

            cabalWrapped
            ghc
          ];
        };
      }
    );
}
