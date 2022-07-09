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

        sedEsc = builtins.replaceStrings ["/"] ["\\/"];
        bash = "${pkgs.bash}/bin/bash";

        deriv = { ssid, psk }: pkgs.stdenv.mkDerivation {
          src = ./.;

          name = "adhoctempmon_dst";
          version = "0.0.1";

          postPatch = ''
            sed -i 's/${sedEsc "/usr/bin/env bash"}/${sedEsc bash}/' build.sh
            sed -i 's/${sedEsc "/usr/bin/env bash"}/${sedEsc bash}/' genconf

            mkdir rootfs
            tar -C rootfs -xf ${rootfs}

            [ -f ./conf ] || cat <<EOF | ./genconf
              ${ssid}
              ${psk}
            EOF
          '';

          buildPhase = ''
            ls
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
      in {
        lib.dist = deriv;

        defaultPackage = deriv {
          ssid = "";
          psk = "";
        };
      }
    );
}
