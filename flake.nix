{
  description = "Flake for the C Worksheet Instrumentor";

  inputs = {
    devenv-root = {
      url = "file+file:///dev/null";
      flake = false;
    };
    devenv = {
      url = "github:cachix/devenv";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    flake-parts.url = "github:hercules-ci/flake-parts";
    flake-utils = {
      url = "github:numtide/flake-utils";
      inputs.systems.follows = "systems";
    };
    gradle2nix = {
      url = "github:tadfisher/gradle2nix/v2";
      inputs = {
        flake-utils.follows = "flake-utils";
        nixpkgs.follows = "nixpkgs";
      };
    };
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    systems.url = "github:nix-systems/default";
    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = {
    self,
    devenv-root,
    devenv,
    flake-parts,
    gradle2nix,
    nixpkgs,
    systems,
    treefmt-nix,
    ...
  } @ inputs:
    flake-parts.lib.mkFlake {inherit inputs;} {
      systems = import systems;

      imports = [
        devenv.flakeModule
        treefmt-nix.flakeModule
      ];

      perSystem = {
        self',
        config,
        pkgs,
        system,
        ...
      }: {
        apps = {
          c-worksheet-instrumentor = {
            type = "app";
            program = "${self'.packages.c-worksheet-instrumentor}/bin/c-worksheet-instrumentor";
          };
          c-worksheet-server = {
            type = "app";
            program = "${self'.packages.c-worksheet-instrumentor}/bin/c-worksheetify-server";
          };
        };

        devenv.shells.default = {pkgs, ...}: {
          devenv.root = let
            devenvRootFileContent = builtins.readFile devenv-root.outPath;
          in
            pkgs.lib.mkIf (devenvRootFileContent != "") devenvRootFileContent;

          # https://github.com/cachix/devenv/issues/528
          containers = pkgs.lib.mkForce {};

          programs.treefmt.package = config.treefmt.build.wrapper;

          imports = [./devenv.nix];
        };

        packages = {
          default = pkgs.callPackage ./c-worksheet-instrumentor.nix {inherit (gradle2nix.builders.${system}) buildGradlePackage;};
        };

        treefmt = import ./treefmt.nix;
      };
    };
}
