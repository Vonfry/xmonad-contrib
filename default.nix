{ mkDerivation, base, bytestring, cairo, containers, directory
, extensible-exceptions, filepath, gi-cairo, gi-pango, mtl
, old-locale, old-time, pango, process, random, stdenv, unix
, utf8-string, X11, X11-xft, xmonad
}:
mkDerivation {
  pname = "xmonad-contrib";
  version = "0.16";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring cairo containers directory extensible-exceptions
    filepath gi-cairo gi-pango mtl old-locale old-time pango process
    random unix utf8-string X11 X11-xft xmonad
  ];
  homepage = "http://xmonad.org/";
  description = "Third party extensions for xmonad";
  license = stdenv.lib.licenses.bsd3;
}
