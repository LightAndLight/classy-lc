{ mkDerivation, base, containers, equivalence, mtl, lens, transformers, stdenv }:
mkDerivation {
  pname = "classy-lc";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base containers equivalence mtl lens transformers ];
  license = stdenv.lib.licenses.bsd3;
}
