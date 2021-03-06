#!/usr/bin/env bash

set -e

ROOTFS_LOC="https://dl-cdn.alpinelinux.org/alpine/v3.16/releases/armhf/alpine-rpi-3.16.0-armhf.tar.gz"
INITRAMFS="./rootfs/boot/initramfs-rpi"

stage() {
    printf "\033[33;1m$1\033[0m\n"
}

warn() {
    printf "\033[36m$1\033[0m\n"
}

ensure_cmd() {
    # Apparently `which` isn't POSIX, let's use `command -v` instead.
    if command -v "$1" >/dev/null 2>&1; then
        printf "\033[32mFound:\t$1\033[0m\n"
    else
        printf "\033[31;1mFatal error, missing: $1\033[0m\n"
	exit 1
    fi
}

ensure_dne() {
    if [ -d "$1" ]; then
	printf "\033[31;1mFatal error, directory present: $1\033[0m\n"
	exit 1
    fi
}

ensure_empty() {
    if [ ! -z "$(ls -A $1)" ]; then
	printf "\033[31;1mFatal error, directory not empty: $1\033[0m\n"
	exit 1
    fi
}

stage "LOADING CONFIG"

. ./conf

stage "CHECKING ENVIRONMENT"

ensure_cmd cpio
ensure_cmd curl
ensure_cmd gunzip
ensure_cmd mktemp
ensure_cmd tar
ensure_dne patched-rootfs

stage "ACQUIRING ROOTFS"

if [ -d ./rootfs ]; then
    warn "Using existing archive in ./rootfs."
else
    warn "Downloading rootfs"
    curl "$ROOTFS_LOC" > rootfs.tar.gz
    mkdir ./rootfs
    tar -xf rootfs.tar.gz -C rootfs
fi

stage "MOUNTING INITRAMFS"

IRFS_UNCOMP=$(mktemp --suffix=".gz")
cp "$INITRAMFS" "$IRFS_UNCOMP"
gunzip "$IRFS_UNCOMP"
IRFS_UNCOMP="${IRFS_UNCOMP%.gz}"
chmod u+rw "$IRFS_UNCOMP"

warn "uncompressed initramfs to $IRFS_UNCOMP"

IRFS_DIR=$(mktemp -d)
CURDIR=$(pwd)

cd "$IRFS_DIR"
cpio -i < $IRFS_UNCOMP 2>/dev/null
cd "$CURDIR"

warn "dearchived initramfs to $IRFS_DIR"

stage "PATCHING SYSTEM"

cp -r rootfs patched-rootfs

# set up the splash screen, AMOGUS
mkdir -p $IRFS_DIR/media/splash/
cp fbsplash0.ppm $IRFS_DIR/media/splash/

warn "set splash screen"

# We need to override the init script to copy over everything in
# /ensure.
cp ./init.sh $IRFS_DIR/init

warn "set init script"

mkdir -p $IRFS_DIR/ensure/etc/
cat <<EOF > $IRFS_DIR/ensure/etc/wpa_supplicant.conf
network={
	ssid="$WIFI_SSID"
	psk="$WIFI_PSK"
}
EOF

mkdir -p $IRFS_DIR/ensure/etc/network/
cat <<EOF > $IRFS_DIR/ensure/etc/network/interfaces
auto lo
auto wlan0
iface wlan0 inet dhcp
EOF

mkdir -p $IRFS_DIR/ensure/etc/conf.d
cat <<EOF > $IRFS_DIR/ensure/etc/conf.d/wpa_supplicant
wpa_supplicant_if="wlan0"
wpa_supplicant_conf="/etc/wpa_supplicant.conf"

# wpa_supplicant will use dbus by default ... on alpine. somehow.
wpa_supplicant_dbus="no"
EOF

warn "configured wifi"

stage "ARCHIVING SYSTEM"

# We're mirroring the process alpine uses for creation in
# mkinitfs[0]. This pipeline is stolen from there.
#
# [0]: https://gitlab.alpinelinux.org/alpine/mkinitfs/-/blob/master/mkinitfs.in
(cd "$IRFS_DIR" && find . | sort | cpio --renumber-inodes -H newc -o 2>/dev/null) | \
    gzip -9 > ./patched-rootfs/boot/initramfs-rpi

rm -rf "$IRFS_DIR"
rm "$IRFS_UNCOMP"

tar -cf tempmon-os.tar -C patched-rootfs .
rm -rf patched-rootfs

printf "\033[32;1m"
printf "DISTRIBUTION PROCESS COMPLETED"
printf "\033[0m\n"
echo "tempmon-os.tar should be extracted onto a bootable SD card. Pi's are"
echo "fickle with what partitions they'll accept. Here's what worked for me:"
echo
echo "* An MBR disk, containing"
echo "* ...a FAT-32 partition"
echo "  * ...made with mkdosfs"
echo "  * ...with a FAT-16 partition type."
