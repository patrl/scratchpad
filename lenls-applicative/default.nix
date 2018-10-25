{ mkDerivation, base, lens, stdenv }:
mkDerivation {
  pname = "dynamic-applicative";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base lens ];
  license = stdenv.lib.licenses.bsd3;
}
