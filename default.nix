{ pkgs ? import <nixpkgs> {} }:

pkgs.callPackage ./c-worksheet-instrumentor.nix { }
