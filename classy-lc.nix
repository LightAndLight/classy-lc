{ mkDerivation, base, equivalence, mtl, lens, stdenv }:
mkDerivation {
  pname = "classy-lc";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base equivalence mtl lens ];
  license = stdenv.lib.licenses.bsd3;
}
