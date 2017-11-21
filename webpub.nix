{ mkDerivation, base, bytestring, containers, directory
, epub-metadata, filepath, hxt, mtl, network-uri, stdenv
, transformers, utf8-string, zip-archive
}:
mkDerivation {
  pname = "webpub";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base bytestring containers directory epub-metadata filepath hxt mtl
    network-uri transformers utf8-string zip-archive
  ];
  description = "Convert to EPUB eBooks to web-publishable sites";
  license = stdenv.lib.licenses.gpl2;
}
