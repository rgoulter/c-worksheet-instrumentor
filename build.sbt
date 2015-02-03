name := "worksheetify-instrumentor"

organization := "edu.nus"

version := "0.1-SNAPSHOT"

scalaVersion := "2.11.2"


// Lib Deps.
libraryDependencies += "edu.nus" % "cdecl_2.11" % "0.1.0-SNAPSHOT"

libraryDependencies += "org.antlr" % "ST4" % "4.0.8"

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test"

// libraryDependencies += "org.osgi" % "org.osgi.core" % "4.3.0" % "provided"

// Antlr
antlr4Settings

antlr4GenListener in Antlr4 := true

antlr4GenVisitor in Antlr4 := true

antlr4PackageName in Antlr4 := Some("edu.nus.worksheet.instrumentor")

javaSource in Antlr4 := (baseDirectory / "target/generated-sources/antlr4").value

antlr4Dependency in Antlr4 := "org.antlr" % "antlr4" % "4.5"


// Ensure Eclipse project classpath has all the folders we want,
// including src/main/antlr4 and its generated folder.
// Resource dir for StringTemplate group files.
unmanagedSourceDirectories in Compile <+= sourceDirectory in Antlr4

EclipseKeys.createSrc := EclipseCreateSrc.Default +
                         EclipseCreateSrc.Resource +
                         EclipseCreateSrc.Managed +
                         EclipseCreateSrc.Unmanaged

// OSGi Bundling
osgiSettings

OsgiKeys.bundleVersion := version.value.replace("SNAPSHOT", "qualifier")

OsgiKeys.exportPackage := Seq("edu.nus.worksheet")

OsgiKeys.privatePackage := Seq("edu.nus.worksheet.instrumentor")

OsgiKeys.additionalHeaders := Map("Bundle-Name" -> "Worksheetify Instrumentor");
