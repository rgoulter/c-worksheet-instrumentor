{
  pkgs ? import <nixpkgs> {},
  gradle2nix ? import (fetchTarball "https://github.com/tadfisher/gradle2nix/archive/v2") {},
}:
pkgs.callPackage ./c-worksheet-instrumentor.nix {inherit (gradle2nix) buildGradlePackage;}
