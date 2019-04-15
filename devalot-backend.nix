{ mkDerivation, aeson, base, bytestring, directory, mime-mail, mtl
, servant, servant-server, stdenv, text, wai, warp
}:
mkDerivation {
  pname = "devalot-backend";
  version = "2.0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring directory mime-mail mtl servant
    servant-server text wai warp
  ];
  executableHaskellDepends = [
    aeson base bytestring directory mime-mail mtl servant
    servant-server text wai warp
  ];
  homepage = "http://www.devalot.com";
  license = stdenv.lib.licenses.bsd2;
}
