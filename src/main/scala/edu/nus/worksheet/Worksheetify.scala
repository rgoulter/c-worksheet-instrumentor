package edu.nus.worksheet

import scala.io._
import java.io._
import scala.sys.process.{ Process, ProcessIO }
import scala.collection.mutable
import scala.collection.mutable.MutableList
import scala.concurrent.{Channel, Promise, promise}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Random;
import java.util.regex.Pattern
import edu.nus.worksheet.instrumentor._

object Worksheetify {

  def processWorksheet(srcLines : Seq[String],
                       outputTo : WorksheetOutput,
                       cc : String = FindCompiler.findCompilerOnPath(),
                       stdinLines : Seq[String] = Seq()) {
    // For worksheet directives (in instrumenting code),
    // we generate a random string so that it becomes more difficult
    // for a program to interfere with the instrumentor.
    val nonce = "_" + (Random.alphanumeric.take(5).mkString);

    def handleIn(output: java.io.OutputStream) {
      val out = new PrintWriter(output);
      stdinLines.foreach(out.println(_));
      out.close();
    }
    
    def handleOut(input: java.io.InputStream) {
      // Regexs to match from the STDOUT of the instrumented program.
      val LineNum = LineDirective(nonce).regex();
      val Worksheet = WorksheetDirective(nonce).regex();
      val FunctionEnter = FunctionEnterDirective(nonce).regex();
      val FunctionReturn = FunctionReturnDirective(nonce).regex();

      // wait for all output from instrumented program :(
      val lines = Source.fromInputStream(input).getLines();
      val currentLineStack = mutable.ArrayStack[Int]();
      currentLineStack.push(0); // lines of source start from 1.
      def currentLine() : Int =
        currentLineStack.top;
      def setCurrentLine(i : Int) =
        currentLineStack(0) = i; // stack begins at 0
      
      for (line <- lines) {
        line match {
          case LineNum(s, d, blockName, blockIteration) => {
            if (s.length() > 0) {
              println(currentLine + ":WS " + s);
              outputTo.addWorksheetOutput(currentLine, s);
            }
            setCurrentLine(d.toInt);
            println(s"LINE: $d in block $blockName iter $blockIteration");
          }
          case Worksheet(s) => {
            println(currentLine + ":WS " + s);
            outputTo.addWorksheetOutput(currentLine, s);
          }
          case FunctionEnter() => {
            currentLineStack.push(-1);
          }
          case FunctionReturn() => {
            currentLineStack.pop();
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
    val originalProgram = new CProgram(inputProgramSrc, cc = cc);
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
    val instrumentedProgram = Instrumentor.instrument(inputProgramSrc, nonce);
    println(instrumentedProgram);
    
    // Output to /tmp/instrument.c
    val writeInstrumentedOutput = new BufferedWriter(new FileWriter("/tmp/instrumented.c"));
    writeInstrumentedOutput.write(instrumentedProgram);
    writeInstrumentedOutput.close();

    val prog = new CProgram(instrumentedProgram, cc = cc);
    val (instrumentedWarnings, instrumentedErrors) = prog.compile();
    assert(instrumentedErrors.isEmpty);
    
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

  for (int i = 0; i < 3; i++) {
    printf("hi\n");
  }

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