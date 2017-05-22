{ mkDerivation, base, dependent-map, dependent-sum, jsaddle, lens
, reflex, reflex-dom, stdenv, text
}:
mkDerivation {
  pname = "myawesomeclient";
  version = "0.0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base dependent-map dependent-sum jsaddle lens reflex reflex-dom
    text
  ];
  executableHaskellDepends = [
    base dependent-map dependent-sum jsaddle lens reflex reflex-dom
    text
  ];
  homepage = "https://github.com/ConferHealth";
  description = "It's a client!";
  license = stdenv.lib.licenses.bsd3;
}
