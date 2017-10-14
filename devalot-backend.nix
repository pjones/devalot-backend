{ mkDerivation, aeson, base, bytestring, case-insensitive, hspec
, HUnit, lens, mime-mail, mtl, snap, snap-core, snap-server, stdenv
, text
}:
mkDerivation {
  pname = "devalot-backend";
  version = "1.0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring lens mime-mail mtl snap snap-core snap-server
    text
  ];
  executableHaskellDepends = [ base bytestring snap snap-server ];
  testHaskellDepends = [
    base bytestring case-insensitive hspec HUnit snap snap-core text
  ];
  homepage = "http://www.devalot.com";
  license = stdenv.lib.licenses.bsd2;
}
