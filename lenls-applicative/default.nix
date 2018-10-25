{ mkDerivation, base, lens, mtl, stdenv, transformers }:
mkDerivation {
  pname = "dynamic-applicative";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base lens mtl transformers ];
  license = stdenv.lib.licenses.bsd3;
}
