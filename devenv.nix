{
  config,
  lib,
  pkgs,
  ...
}: {
  options = {
    programs.treefmt = {
      package = lib.mkOption {
        defaultText = lib.literalMD "package for running `treefmt` in devshell";
      };
    };
  };

  config = {
    packages = with pkgs; [
      scalafix
      config.programs.treefmt.package
    ];

    languages = {
      java.gradle.enable = true;
      scala.enable = true;
    };
  };
}
