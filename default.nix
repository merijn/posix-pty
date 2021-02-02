{ pkgs ? import <nixpkgs> {}, args ? {} }:

pkgs.haskell.lib.dontCheck (pkgs.haskellPackages.callCabal2nix "posix-pty" ./. args)
