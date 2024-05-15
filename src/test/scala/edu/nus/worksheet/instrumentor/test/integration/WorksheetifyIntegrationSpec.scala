package edu.nus.worksheet.instrumentor.test.integration

import java.nio.file.{Files, Path, Paths}
import java.nio.charset.StandardCharsets

import scala.io.Source
import scala.sys.process._

import org.scalatest._
import flatspec._

import edu.nus.worksheet._
import edu.nus.worksheet.instrumentor._

class WorksheetifyIntegrationSpec extends AnyFlatSpec {

  def createTempFileWithContents(contents: String): Path = {
    val tempFile = Files.createTempFile("temp", ".txt")
    Files.write(tempFile, contents.getBytes(StandardCharsets.UTF_8))
    tempFile
  }

  "c-worksheet-instrumentor" should "run the hello.c snapshot" taggedAs(IntegrationTest) in {
    val snapshot = "hello.c";

    val inputProgram = Source.fromResource(f"snapshots/${snapshot}").mkString;
    val expectedOutput = Source.fromResource(f"snapshots/${snapshot}.expected").mkString;

    val installedBinPath = Paths.get("build/install/c-worksheet-instrumentor/bin/c-worksheet-instrumentor");

    val inputProgramPath = createTempFileWithContents(inputProgram);

    val command: Seq[String] =
      if (System.getProperty("os.name").toLowerCase.contains("windows"))
        Seq("cmd", "/C", s"${installedBinPath.toString}.bat", inputProgramPath.toString)
      else
        Seq(installedBinPath.toString, inputProgramPath.toString)

    val actualOutput = Process(command).!!

    assertResult(expectedOutput)(actualOutput);
  }

}
