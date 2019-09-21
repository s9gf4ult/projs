{ nixpkgs ? import ./nixpkgs.nix }:

nixpkgs.haskellPackages.callPackage ./default.nix { }
