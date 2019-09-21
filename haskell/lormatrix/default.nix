{ mkDerivation, accelerate, accelerate-llvm-native, array, base
, clock, repa, stdenv
}:
mkDerivation {
  pname = "lormatrix";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    accelerate accelerate-llvm-native array base clock repa
  ];
  license = stdenv.lib.licenses.bsd3;
}
