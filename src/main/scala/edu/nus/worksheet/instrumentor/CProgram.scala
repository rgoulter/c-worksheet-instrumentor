package edu.nus.worksheet.instrumentor;

import java.io.File;
import java.util.concurrent.LinkedTransferQueue;
import java.util.regex.Pattern;

import scala.collection.mutable.ListBuffer;
import scala.concurrent.Promise;
import scala.io.Source;
import scala.language.postfixOps;
import scala.sys.process.{ProcessBuilder, ProcessIO, Process};

abstract class Diagnostic(
    val source: String,
    val line: Int,
    val column: Int,
    val message: String
) {
  def diagnosticMessage(): String =
    s"$source:$line:$column: $message";
}

object Diagnostic {
  val Warning = "([^:]+):(\\d+):(\\d+): warning: (.*)".r;
  val Error = "([^:]+):(\\d+):(\\d+): error: (.*)".r;
}

case class WarningMessage(
    wsource: String,
    wline: Int,
    wcol: Int,
    wmessage: String
) extends Diagnostic(wsource, wline, wcol, wmessage);

case class ErrorMessage(
    esource: String,
    eline: Int,
    ecol: Int,
    emessage: String
) extends Diagnostic(esource, eline, ecol, emessage);

class CProgram(
    var inputProgram: String,
    var cc: String = FindCompiler.findCompilerOnPath(),
    val additionalFlags: Iterable[String] = Seq(),
    val macroDefinitions: Map[String, String] = Map()
) {

  val UseC99Standard = "-std=c99";
  val StopAfterPreprocessing = "-E";
// val SuppressDiagnosticCaret = "-fno-diagnostics-show-caret";
  val SuppressDiagnosticCaret = "";
  val SuppressDiagnosticFlag = "-fno-diagnostics-show-option";
  val ReadCFromStdIn = "-xc -";

  // Use a temporary file for our CProgram,
  // so that multiple CPrograms can be used at the same time.
  val tmpFile = File.createTempFile("cworksheet", ".out");
  tmpFile.deleteOnExit();

  def programPath(): String = tmpFile.getAbsolutePath();

  def checkForErrors(): (Seq[WarningMessage], Seq[ErrorMessage]) =
    // Compiling is cheap.
    compile();

  private[CProgram] def handleIn(output: java.io.OutputStream): Unit = {
    // Write our input string to the process' STDIN
    val writer = new java.io.PrintWriter(output);
    // writer.write(inputProgram);
    writer.write(inputProgram);
    writer.close();
  }

  // Try pre-processing the given C program,
  // returning String if there were no errors.
  def preprocessed(): Option[String] = {
    val compileCommand = (Seq(
      cc,
      UseC99Standard,
      StopAfterPreprocessing,
      SuppressDiagnosticCaret,
      SuppressDiagnosticFlag
    ) ++
      additionalFlags.toSeq ++
      macroDefinitions.map({ case (k, v) => s"-D$k=$v" }) :+
      ReadCFromStdIn).mkString(" ");

    val outputChannel = new LinkedTransferQueue[String]();

    def handleOut(input: java.io.InputStream): Unit = {
      val ccOut = Source.fromInputStream(input).mkString;
      outputChannel.put(ccOut);
    }

    val processIO = new ProcessIO(handleIn, handleOut, { _ => () });
    val compileResult = Process(compileCommand).run(processIO).exitValue();

    return if compileResult != 0 then {
      None;
    } else {
      Some(outputChannel.take());
    }
  }

  def compile(): (Seq[WarningMessage], Seq[ErrorMessage]) = {
    val outputPath = s"-o ${programPath()}";

    val compileCommand = (Seq(
      cc,
      UseC99Standard,
      outputPath,
      SuppressDiagnosticCaret,
      SuppressDiagnosticFlag
    ) ++
      additionalFlags.toSeq ++
      macroDefinitions.map({ case (k, v) => s"-D$k=$v" }) :+
      ReadCFromStdIn).mkString(" ");

    val diagnosticsChannel =
      new LinkedTransferQueue[(Seq[WarningMessage], Seq[ErrorMessage])]();

    def handleOut(input: java.io.InputStream): Unit = {
      val ccOut = Source.fromInputStream(input).mkString;
    }

    def handleErr(input: java.io.InputStream): Unit = {
      // Process lines which are of form:
      // <source>:<line>:<char>: (error|warning): <message>
      val warnings = new ListBuffer[WarningMessage]();
      val errors = new ListBuffer[ErrorMessage]();

      for err <- Source.fromInputStream(input).getLines() do {
        err match {
          case Diagnostic.Warning(src, line, col, msg) => {
            warnings += WarningMessage(src, line.toInt, col.toInt, msg);
          }
          case Diagnostic.Error(src, line, col, msg) => {
            errors += ErrorMessage(src, line.toInt, col.toInt, msg);
          }
          case _ => ();
        }
      }

      diagnosticsChannel.put((warnings.toSeq, errors.toSeq));
    }

    val processIO = new ProcessIO(handleIn, handleOut, handleErr);
    val compileResult = Process(compileCommand).run(processIO).exitValue();

    return diagnosticsChannel.take();
  }

  // The ProcessBuilder of the compiled CProgram.
  def process(): ProcessBuilder = Process(programPath());
}
