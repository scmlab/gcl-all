{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    oldNixpkgs.url = "github:NixOS/nixpkgs/release-21.11";
    utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, oldNixpkgs, utils }: utils.lib.eachDefaultSystem (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};
      oldPkgs = oldNixpkgs.legacyPackages.${system};
      hPkgs = oldPkgs.haskell.packages."ghc8107";

      devTools = with hPkgs; [
        ghc
        haskell-language-server

        stack-wrapped
        pkgs.zlib
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
