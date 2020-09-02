{ pkgs ? import <nixpkgs> {} }:

let
  xmonad-contrib = p: p.callPackage ./. {};
in
pkgs.haskellPackages.shellFor {
  packages = p: [ (xmonad-contrib p) ];
  withHoogle = true;
  buildInputs = with pkgs; [
    cabal2nix
  ];
}
