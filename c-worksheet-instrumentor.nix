{
  lib,
  makeWrapper,
  buildGradlePackage,
  jdk,
  coreutils,
  findutils,
  gnused,
  gcc,
}:
buildGradlePackage rec {
  pname = "c-worksheet-instrumentor";
  version = lib.strings.removeSuffix "\n" (builtins.readFile ./version);
  lockFile = ./gradle.lock;
  src = builtins.path {
    path = ./.;
    name = pname;
  };
  buildInputs = [makeWrapper];
  gradleFlags = ["build" "testIntegration" "installDist"];
  installPhase = let
    gradleDistScriptDeps = [coreutils findutils gnused];
  in ''
    mkdir -p $out
    cp -r build/install/${pname}/* $out

    rm $out/bin/*.bat

    wrapProgram $out/bin/c-worksheet-instrumentor \
      --set JAVA_HOME ${jdk.home} \
      --suffix PATH : ${lib.makeBinPath (gradleDistScriptDeps ++ [gcc])}
    wrapProgram $out/bin/c-worksheetify-server \
      --set JAVA_HOME ${jdk.home} \
      --suffix PATH : ${lib.makeBinPath (gradleDistScriptDeps ++ [gcc])}
  '';
}
