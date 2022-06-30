{
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  inputs.rootfs = {
    # Load this as a file; the tarball structure doesn't mesh with
    # what Nix expects.
    type = "file";
    url =
      "https://dl-cdn.alpinelinux.org/alpine/v3.16/releases/armhf/alpine-rpi-3.16.0-armhf.tar.gz";
    flake = false;
  };

  outputs = { self, nixpkgs, flake-utils, rootfs }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
      in {
        defaultPackage = pkgs.stdenv.mkDerivation {
          name = "adhoctempmon_dst";
          version = "0.0.1";
          
          src = ./.;

          # This isn't a patch, but, unlike some other phases, it
          # actually modifies the source tree.
          postPatch = ''
            mkdir rootfs
            tar -C rootfs -xf ${rootfs}
          '';

          buildPhase = ''
            ./build.sh
          '';

          installPhase = ''
            mkdir -p $out
            cp ./tempmon-os.tar $out
          '';

          doUnpack = true;
          dontConfigure = true;

          buildInputs = with pkgs; [ cpio curl ];
        };
      }
    );
}
