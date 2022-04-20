{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  inputs.chip8-roms = {
    url = "github:loktar00/chip8";
    flake = false;
  };

  outputs = { self, nixpkgs, flake-utils, chip8-roms }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs { inherit system; };

        ghc = pkgs.haskell.packages.ghc8107.ghcWithPackages (
          ps: [
            ps.ansi-terminal
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
      rec {
        defaultPackage = packages.haskell-chip8;
        packages = rec {
          haskell-chip8 = pkgs.haskellPackages.callCabal2nix "haskell-chip8" ./. { };
          ibm-test = pkgs.writeShellScriptBin "ibm-test" ''
            exec "${haskell-chip8}/bin/haskell-chip8" "${chip8-roms}/roms/IBM Logo.ch8"
          '';
          pong-test = pkgs.writeShellScriptBin "pong-test" ''
            exec "${haskell-chip8}/bin/haskell-chip8" "${chip8-roms}/roms/Pong (alt).ch8"
          '';
        };

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
