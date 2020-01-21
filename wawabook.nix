{ mkDerivation, base, bytestring, conduit, http-conduit, resourcet
, stdenv, tagsoup, time, transformers
}:
mkDerivation {
  pname = "wawabook";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base bytestring conduit http-conduit resourcet tagsoup time
    transformers
  ];
  description = "A HTML scraper for wawabook.com.tw";
  license = stdenv.lib.licenses.asl20;
}
