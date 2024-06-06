{
  description = "Flake for the C Worksheet Instrumentor";

  inputs = {
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
        treefmt-nix.flakeModule
      ];

      perSystem = {
        config,
        pkgs,
        system,
        ...
      }: {
        apps = {
          c-worksheet-instrumentor = {
            type = "app";
            program = "${self.packages.${system}.c-worksheet-instrumentor}/bin/c-worksheet-instrumentor";
          };
          c-worksheet-server = {
            type = "app";
            program = "${self.packages.${system}.c-worksheet-instrumentor}/bin/c-worksheetify-server";
          };
        };

        devShells = {
          default = devenv.lib.mkShell {
            inherit inputs pkgs;

            modules = [
              (import ./devenv.nix)
            ];
          };
        };

        packages = {
          default = self.packages.${system}.c-worksheet-instrumentor;

          c-worksheet-instrumentor = pkgs.callPackage ./c-worksheet-instrumentor.nix {inherit (gradle2nix.builders.${system}) buildGradlePackage;};
        };

        treefmt = import ./treefmt.nix;
      };
    };
}
