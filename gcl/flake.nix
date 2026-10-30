{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    # pinned against this specific commit so we don't have to rebuild GHC from scratch
    nixpkgs-005433b.url = "github:NixOS/nixpkgs/005433b926e16227259a1843015b5b2b7f7d1fc3";
    utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, nixpkgs-005433b, utils }: utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs {
        inherit system;
      };

      pkgs-005433b = import nixpkgs-005433b {
        inherit system;
      };

      hPkgs = pkgs-005433b.haskell.packages."ghc984";

      devTools = [
        stack-wrapped
        pkgs.z3
        pkgs.zlib
        pkgs.ormolu

        hPkgs.ghc
        hPkgs.haskell-language-server
      ];

      stack-wrapped = pkgs.symlinkJoin {
        name = "stack"; # will be available as the usual `stack` in terminal
        paths = [ pkgs.stack ];
        buildInputs = [ pkgs.makeWrapper ];
        postBuild = ''
          wrapProgram $out/bin/stack \
            --add-flags "\
              --no-nix \
              --system-ghc \
              --no-install-ghc \
            "
        '';
      };
    in
    {
      devShell = pkgs.mkShell {
        buildInputs = devTools;

        LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath devTools;
      };
    }
  );
}
