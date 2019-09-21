let
  oldNixpkgs = import <nixpkgs> {} ;
  inherit (oldNixpkgs) fetchFromGitHub lib;
  nixpkgsPath = fetchFromGitHub (
    lib.importJSON ./nixpkgs.json
  );
  _nixpkgs = import nixpkgsPath {
    config = { allowBroken = true; };
  };
  nixpkgs =
    let
      inherit (_nixpkgs.haskell.lib) dontCheck;
    in
    _nixpkgs // {
      haskellPackages = _nixpkgs.haskellPackages.override {
        overrides = self: super: {
          accelerate-llvm-native = dontCheck super.accelerate-llvm-native;
        };
      };
    };
in nixpkgs
