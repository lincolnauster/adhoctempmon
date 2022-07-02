{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "aarch64-linux" ] (system:
      let
        pkgs = (import nixpkgs {
          inherit system;
          crossSystem.config = "aarch64-linux";
        }).pkgsStatic;

        nativePkgs = import nixpkgs {
          inherit system;
        };

        defaultPackage = pkgs.haskellPackages.callPackage ./default.nix {};

        nativeDefaultPackage =
          nativePkgs.haskellPackages.callPackage ./default.nix {};
      in {
        inherit defaultPackage;
        devShell = nativePkgs.haskellPackages.shellFor {
          packages = p: [ nativeDefaultPackage ];
          buildInputs = with pkgs; [ cabal-install ];
        };
      });
}
