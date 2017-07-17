{ mkDerivation, aeson, base, bytestring, stdenv, text, time
, unordered-containers
}:
mkDerivation {
  pname = "haskube";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bytestring text time unordered-containers
  ];
  homepage = "https://github.com/ublubu/haskube#readme";
  license = stdenv.lib.licenses.bsd3;
}
