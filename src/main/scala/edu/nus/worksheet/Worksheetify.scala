package edu.nus.worksheet

import scala.io._
import java.io._
import scala.sys.process.{ Process, ProcessIO }
import scala.collection.mutable
import scala.collection.mutable.MutableList
import scala.concurrent.{Channel, Promise, promise}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import java.util.regex.Pattern
import edu.nus.worksheet.instrumentor._

object Worksheetify {

  def processWorksheet(srcLines : List[String], outputTo : WorksheetOutput) {
    def handleIn(output: java.io.OutputStream) {
      // For now, the augmented programs don't accept STDIN.
      // This is not a good limitation.
      output.close();
    }
    
    def handleOut(input: java.io.InputStream) {
      /*
       * SCHEMA:
       * LINE <number>    ::= instrumented program is currently at the given line.
       * WORKSHEET <line> ::= output generated by our worksheet instrumentation.
       */
      val LineNum = "LINE (\\d+)".r
      val Worksheet = "WORKSHEET (.*)".r

      // wait for all output from instrumented program :(
      val lines = Source.fromInputStream(input).getLines();
      var currentLine = 0; // lines of source start from 1.
      
      for (line <- lines) {
        line match {
          case LineNum(d) => currentLine = d.toInt;
          case Worksheet(s) => {
            println(currentLine + ":WS " + s);
            outputTo.addWorksheetOutput(currentLine, s);
          }
          case s => {
            println(currentLine + ":" + line);
            outputTo.addLineOfOutput(currentLine, line);
          }
        }
      }

      // No more output coming from instrumented program,
      // so worksheet has no more output to give.
      outputTo.close();
    }
    
    def handleErr(input: java.io.InputStream) {
      val ccErr = Source.fromInputStream(input).mkString;
      if (ccErr.length > 0)
        println("Instrumented Program STDERR:" + ccErr);
    }
    
    // If Program has errors, we can correspond these errors
    // and return *that* as the output.
    println("Checking...");
    val inputProgramSrc = srcLines.mkString("\n");
    val originalProgram = new CProgram(inputProgramSrc);
    val (inputWarnings, inputErrors) = originalProgram.checkForErrors();

    if (!inputErrors.isEmpty) {
      println("There were errors! Stopping.");

      def messageFor(d : Diagnostic) : String =
        s"${d.line}:${d.column}: ${d.message}";

      inputErrors.foreach({ error => outputTo.addErrorMessage(error.line, messageFor(error)); });
      inputWarnings.foreach({ warning => outputTo.addWarningMessage(warning.line, messageFor(warning)); });
      outputTo.close();

      return;
    }

    println("Instrumenting...");
    val instrumentedProgram = Instrumentor.instrument(inputProgramSrc);
    println(instrumentedProgram);
    
    // Output to /tmp/instrument.c
    val writeInstrumentedOutput = new BufferedWriter(new FileWriter("/tmp/instrumented.c"));
    writeInstrumentedOutput.write(instrumentedProgram);
    writeInstrumentedOutput.close();

    val prog = new CProgram(instrumentedProgram);
    prog.compile();
    
    println("Running...");
    println("$ " + prog.programPath());

    val processIO = new ProcessIO(handleIn, handleOut, handleErr);
    val proc = prog.process().run(processIO);
  }
  


  def main(args : Array[String]) : Unit = {
    val inputProgram = """#include <stdio.h>

int main(int argc, char* argv) {
  int x;
  int arr[5];
  int *ptrToInt;

  printf("Line1\nLine 2\n");
  printf("Another line\n");
}""";
    val inputLines = inputProgram.lines.toList;
    
    val wsOutput = new WorksheetOutput();
    processWorksheet(inputLines, wsOutput);
    val wsOutputStr = wsOutput.generateWorksheetOutput(inputLines);
    println("WORKSHEET WITH OUTPUT:");
    println(wsOutputStr);
  }
}