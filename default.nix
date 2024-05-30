{
  pkgs ? import <nixpkgs> {},
  gradle2nix ? import (fetchTarball "https://github.com/rgoulter/gradle2nix/archive/v2--always-set-gradle-user-home.tar.gz") {},
}:
pkgs.callPackage ./c-worksheet-instrumentor.nix {inherit (gradle2nix) buildGradlePackage;}
