{
  pkgs,
  config,
  ...
}: {
  packages = with pkgs; [
    scalafix
  ];

  languages = {
    java.gradle.enable = true;
    scala.enable = true;
  };
}
