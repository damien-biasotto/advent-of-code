{ mkDerivation, base, bytestring, containers, cookie
, cryptohash-md5, directory, http-client, http-client-tls
, http-types, lib, split, text
}:
mkDerivation {
  pname = "advent-of-code";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring containers cookie cryptohash-md5 directory
    http-client http-client-tls http-types split text
  ];
  executableHaskellDepends = [
    base bytestring containers cookie cryptohash-md5 directory
    http-client http-client-tls http-types split text
  ];
  testHaskellDepends = [
    base bytestring containers cookie cryptohash-md5 directory
    http-client http-client-tls http-types split text
  ];
  doHaddock = false;
  license = lib.licenses.mit;
  mainProgram = "year2015";
}
