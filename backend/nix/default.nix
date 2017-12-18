{ mkDerivation, aeson, aeson-pretty, base, base64-bytestring
, binary-serialise-cbor, bytestring, cereal, containers, directory
, filepath, hspec, parsec, QuickCheck, split, stdenv, strict, text
, unordered-containers, vector
}:
mkDerivation {
  pname = "shoebox";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson aeson-pretty base base64-bytestring binary-serialise-cbor
    bytestring cereal containers directory filepath parsec split strict
    text unordered-containers vector
  ];
  executableHaskellDepends = [ base bytestring text ];
  testHaskellDepends = [ base containers hspec QuickCheck ];
  license = stdenv.lib.licenses.unfree;
}
