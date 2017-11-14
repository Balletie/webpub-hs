{ mkDerivation, base, stdenv }:
mkDerivation {
  pname = "webpub";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base ];
  description = "Convert to EPUB eBooks to web-publishable sites";
  license = stdenv.lib.licenses.gpl2;
}
