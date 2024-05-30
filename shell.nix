{
  pkgs ? import <nixpkgs> {},
  gradle2nix ? import (fetchTarball "https://github.com/rgoulter/gradle2nix/archive/v2--always-set-gradle-user-home.tar.gz") {},
}:
pkgs.mkShell {
  inputsFrom = [(pkgs.callPackage ./default.nix {inherit pkgs gradle2nix;})];
  packages = [
    pkgs.metals
    pkgs.scalafix
    pkgs.scalafmt
  ];
}
