{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  inputsFrom = [(pkgs.callPackage ./c-worksheet-instrumentor.nix {})];
  packages = [
    pkgs.metals
    pkgs.scalafix
    pkgs.scalafmt
  ];
}
