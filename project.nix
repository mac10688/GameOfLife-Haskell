{ mkDerivation, ansi-terminal, base, hspec, ncurses
, reactive-banana, stdenv
}:
mkDerivation {
  pname = "GameOfLife-Haskell";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base ];
  executableHaskellDepends = [
    ansi-terminal base ncurses reactive-banana
  ];
  testHaskellDepends = [ base hspec ];
  homepage = "https://github.com/githubuser/GameOfLife-Haskell#readme";
  description = "Simple project template from stack";
  license = stdenv.lib.licenses.bsd3;
}
