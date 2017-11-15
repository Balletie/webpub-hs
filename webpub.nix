{ mkDerivation, base, bytestring, epub-metadata, mtl, stdenv
, zip-archive
}:
mkDerivation {
  pname = "webpub";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base bytestring epub-metadata mtl zip-archive
  ];
  description = "Convert to EPUB eBooks to web-publishable sites";
  license = stdenv.lib.licenses.gpl2;
}
