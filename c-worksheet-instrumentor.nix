{ lib
, stdenv
, makeWrapper
, substituteAll
, gradle_8
, scala_2_12
, jdk
, gcc
, coreutils
, findutils
, gnused
, perl
}:

stdenv.mkDerivation (finalAttrs: {
  pname = "c-worksheet-instrumentor";
  version = "0.3";

  src = builtins.path { path = ./.; name = finalAttrs.pname; };

  patches = [
    # use `deps` maven repo so that gradle can build offline.
    (substituteAll {
      src = ./use-fod-maven-repo.patch;
      inherit (finalAttrs) deps;
    })
  ];

  # helper build to pre-download deps into fixed-output derivation
  deps = stdenv.mkDerivation {
    name = "${finalAttrs.pname}-${finalAttrs.version}-deps";
    inherit (finalAttrs) src;

    nativeBuildInputs = [
      gradle_8
      perl
    ];

    buildPhase = ''
      runHook preBuild

      # c-worksheet-instrumentor spec tests implicitly assume gcc
      export PATH="${gcc}/bin:$PATH"

      export GRADLE_USER_HOME=$(mktemp -d)
      gradle --no-daemon --console=plain build

      runHook postBuild
    '';

    # perl code mavenizes pathes (com.example.foo/foo/1.0.0/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa/foo-1.0.0.jar -> com/example/foo/foo/1.0.0/foo-1.0.0.jar)
    installPhase = ''
      runHook preInstall

      find $GRADLE_USER_HOME/caches/modules-2 -type f -regex '.*\.\(jar\|pom\)' \
        | perl -pe 's#(.*/([^/]+)/([^/]+)/([^/]+)/[0-9a-f]{30,40}/([^/\s]+))$# ($x = $2) =~ tr|\.|/|; "install -Dm444 $1 \$out/$x/$3/$4/$5" #e' \
        | sh

      runHook postInstall
    '';

    # Remember to update FOD outputHash whenever build.gradle dependencies change
    outputHashAlgo = "sha256";
    outputHashMode = "recursive";
    outputHash = "sha256-07WSxFn73+inahWDdxxs9h7ybtU/ZkpNNlz2HVUqZEg=";
  };

  nativeBuildInputs = [
    makeWrapper
    gradle_8
    scala_2_12
  ];

  buildPhase = ''
    runHook preBuild

    # c-worksheet-instrumentor spec tests implicitly assume gcc
    export PATH="${gcc}/bin:$PATH"

    export GRADLE_USER_HOME=$(mktemp -d)
    gradle --offline --no-daemon --console=plain build

    runHook postBuild
  '';

  installPhase =  let
    gradleDistScriptDeps = [ coreutils findutils gcc gnused ];
  in ''
    runHook preInstall

    gradle --offline --no-daemon installDist

    mkdir -p $out/
    cp -r build/install/${finalAttrs.pname}/* $out/
    rm $out/bin/*.bat

    wrapProgram $out/bin/c-worksheet-instrumentor \
      --set JAVA_HOME ${jdk.home} \
      --prefix PATH : ${lib.makeBinPath (gradleDistScriptDeps ++ [ gcc ])}
    wrapProgram $out/bin/c-worksheetify-server \
      --set JAVA_HOME ${jdk.home} \
      --prefix PATH : ${lib.makeBinPath (gradleDistScriptDeps ++ [ gcc ])}

    runHook postInstall
  '';
})
