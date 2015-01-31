name := "antlr-scala-c"

version := "0.1.3"

scalaVersion := "2.11.2"

antlr4Settings

antlr4GenListener in Antlr4 := true

antlr4GenVisitor in Antlr4 := true

antlr4Dependency in Antlr4 := "org.antlr" % "antlr4" % "4.5"

libraryDependencies += "org.antlr" % "ST4" % "4.0.8"
