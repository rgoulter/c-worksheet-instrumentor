name := "antlr-scala-c"

version := "0.1.3"

scalaVersion := "2.11.2"

antlr4Settings

antlr4GenListener in Antlr4 := true

antlr4GenVisitor in Antlr4 := true

// Antlr4 4.5-RC
// https://groups.google.com/forum/#!topic/antlr-discussion/OPYAVSVkrxM
resolvers += "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/"

antlr4Dependency in Antlr4 := "org.antlr" % "antlr4" % "4.5-SNAPSHOT"

antlr4Dependency in Antlr4 := "org.antlr" % "antlr4-runtime" % "4.5-SNAPSHOT"
