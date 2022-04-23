{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  inputs.chip8-roms = {
    url = "github:loktar00/chip8";
    flake = false;
  };

  inputs.chip8-test-rom = {
    url = "github:corax89/chip8-test-rom";
    flake = false;
  };

  inputs.chip8-test-rom2 = {
    url = "github:daniel5151/AC8E";
    flake = false;
  };

  outputs = { self, nixpkgs, flake-utils, chip8-roms, chip8-test-rom, chip8-test-rom2 }:
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

        mkTestRomScript = scriptName: haskell-chip8: romPath:
          pkgs.writeShellScriptBin scriptName ''
            exec "${haskell-chip8}/bin/haskell-chip8" "${romPath}" "$@"
          '';
      in
      rec {
        defaultPackage = packages.haskell-chip8;
        packages = rec {
          haskell-chip8 = pkgs.haskellPackages.callCabal2nix "haskell-chip8" ./. { };

          ibm-test = mkTestRomScript "ibm-test" haskell-chip8 "${chip8-roms}/roms/IBM Logo.ch8";
          opcode-test = mkTestRomScript "opcode-test" haskell-chip8 "${chip8-test-rom}/test_opcode.ch8";
          opcode-test2 = mkTestRomScript "opcode-test2" haskell-chip8 "${chip8-test-rom2}/roms/bc_test.ch8";
          pong-test = mkTestRomScript "pong-test" haskell-chip8 "${chip8-roms}/roms/Pong (alt).ch8";
          space-invaders-test = mkTestRomScript "space-invaders-test" haskell-chip8 "${chip8-test-rom2}/roms/games/INVADERS";
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
