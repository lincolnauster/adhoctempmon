# a nameless ad-hoc temperature monitor

Or: A Linux distro because existing solutions to some narrow problem
are broken messes of JavaScript.

## A Note on Completion

This isn't yet finished, I just want it in Git, and if it's in Git, it
might as while be published. Some of this README is actually just
wrong and in progress.

## Background

As it turns out, vaccines need to be kept very cold, and if they end
up not-cold, it becomes a problem. People who deal with vaccines have
very fancy freezers that keep vaccines cold the vast majority of the
time. This is all well and good, but the freezer isn't always
perfectly reliable. If something goes wrong with the freezer, the
freezer administrator must be notified. That's what this repository is
for. It's a collection of code and infrastructure that turns a
Raspberry Pi attached to a thermistor into a proper temperature
monitor and alert system.

This repository stores all the software involved in connecting the
thermistor I sliced off an actual thermometer to the internet. All
this code is public, but highly customized to my environment. If
you're trying to replicate this, this README *should* provide enough
detail to do everything you need to do, but open an issue or otherwise
get in contact if there are any ambiguities.

## Overview

IoT is an unreliable mess that's hard to get right. I prefer to avoid
it whenever possible. This happens to be the one-in-a-million time
when you actually _do_ need to connect your freezer to the internet.

This project attempts to make this as simple and foolproof as
possible. It consists of a Raspberry Pi Zero W running Alpine Linux, a
web server, and an I2C monitor for a thermistor. The goal is for the
Pi to contain as little state as possible, and not accumulate any over
its lifetime. To that end, the OS distribution solely exists in
Alpine's initramfs, and it's all erased on reboot. Settings that
strictly must persist have their own partition, but it's totally
separate from our statically-generated OS configuration.

Hardware aside, the build leverages Nix to be simple and
reproducible. After providing a configuration, `nix build` produces a
`tempmon.iso` ready to be flashed onto a Pi and booted. Because Nix,
this ISO is guaranteed to be identical on every other build on every
other machine.

## Hardware Preparation

The ad-hoc temperature monitor is extremely ad-hoc. This section is
less instruction and more documentation of what I have done; there are
undoubtedly things that may be improved here.

Regardless, a thermistor was sliced off of an actual thermometer[0]
and soldered into a voltage divider. At that point, a 10-bit I2C ADC
was attached[1] to both the voltage divider and one of the Pi's GPIO
signal pins, as well attached to the 3.3V power supply and ground the
Pi provides.

[0]: A DeltaTrak ... something or other.

[1]: This is important, the software in this repository *will not work without significant modification* unless the ADC communicates 10-bit information over I2C.

## Software Build Procedure

There are a variety of distinct components:

+ An Alpine-derivative distribution (Shell, `./dist`)
+ A web server (Haskell, `/srv`)
+ A GPIO I2C monitor (C, `/gpio`)

You'll probably need to cross compile *everything*. You *can* [do this
manually](ghc-c), or you can avoid all of that and simply `nix build`
yourself a monitor, like so:

```sh
$ nix build
```

Note that this will take *forever.* The image contains a web service
written in Haskell, which must be statically compiled. Nix doesn't
cache static builds for aarch64, so you'll be bootstrapping the
compiler atop compiling cabal atop every single other
dependency. Bonus points: it's cross compiling! Make yourself a cup of
tea, catch up on email, get outside, and maybe a restful night's sleep
while you wait for this to finish.

After it does, though, you'll end up with a `tempmon.iso` in the
`result` directory, which you can flash onto your SD card:

```sh
$ sudo dd if=./result/tempmon.iso of=/dev/$YOUR_DEVICE status=progress
$ sync
```

This ISO contains a read only boot partition containing an initramfs
with everything the temperature monitor needs, and a very small
persistent state partition containing all the settings specified
earlier.

### Manual Building

#### Web Server

This bit is written in Haskell. Static cross compiling is [a mess](ghc-c),
which, again, you can do manually or via the included Nix derivation:


```sh
$ cd ./srv
$ nix build
```

Note that if you intend to cross-compile manually (i.e., without Nix),
the resulting binary *must be statically linked*:

```sh
$ ldd ./result/bin/srv
ldd: ./result/bin/srv: Not a valid dynamic program

$ file ./result/bin/srv
./result/bin/srv: ELF 64-bit LSB executable, ARM aarch64, version 1 (SYSV), statically linked, stripped
```

[ghc-c]: https://gitlab.haskell.org/ghc/ghc/-/wikis/building/cross-compiling

#### GPIO Monitor

This is ISO C99, and can cross compile with relative simplicity. TODO.

#### rootfs

Then, you'll want to build the rootfs. First, generate your config
with the `genconf` script. This'll ask you for things like your Wifi
credentials, the path to the server binary, and a TLS certificate so
that these can be built into the distribution.

```sh
$ cd dist
$ ./genconf conf
# output snipped
$ ./build conf -o rootfs
# output snipped
```

With that done, you'll become the pround & painful owner of all the
software required to monitor temperatures.
