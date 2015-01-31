name := "antlr-scala-c"

version := "0.1-RC"

scalaVersion := "2.11.2"


// Lib Deps.
libraryDependencies += "org.antlr" % "ST4" % "4.0.8"

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test"


// Antlr
antlr4Settings

antlr4GenListener in Antlr4 := true

antlr4GenVisitor in Antlr4 := true

antlr4Dependency in Antlr4 := "org.antlr" % "antlr4" % "4.5"


// Ensure Eclipse project classpath has all the folders we want,
// including src/main/antlr4 and its generated folder.
// Resource dir for StringTemplate group files.
unmanagedSourceDirectories in Compile <+= sourceDirectory in Antlr4

EclipseKeys.createSrc := EclipseCreateSrc.Default +
                         EclipseCreateSrc.Resource +
                         EclipseCreateSrc.Managed +
                         EclipseCreateSrc.Unmanaged
