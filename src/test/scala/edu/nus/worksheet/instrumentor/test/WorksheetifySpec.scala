package edu.nus.worksheet.instrumentor.test

import org.scalatest._
import edu.nus.worksheet._
import edu.nus.worksheet.instrumentor._

class WorksheetifySpec extends FlatSpec {

  "Worksheetify" should "output info for declarations" in {
    val inputProgram = """#include <stdio.h>

int main(int argc, char* argv) { // Line 03
  int x;
}""";
    val inputLines = inputProgram.lines.toList;
    
    val wsOutput = new WorksheetOutput();
    Worksheetify.processWorksheet(inputLines, wsOutput);
    val wsOutputStr = wsOutput.generateWorksheetOutput(inputLines); // block until done.
    
    wsOutput.outputPerLine.get(4) match {
      case Some(actual) => assert(actual.length > 0);
      case None => fail("No output was given.");
    }
  }



  it should "output info for assignments, on the correct line." in {
    val inputProgram = """#include <stdio.h>

int main(int argc, char* argv) { // Line 03
  int x;
  x = 5;
  x = 6;
}""";
    val inputLines = inputProgram.lines.toList;
    
    val wsOutput = new WorksheetOutput();
    Worksheetify.processWorksheet(inputLines, wsOutput);
    val wsOutputStr = wsOutput.generateWorksheetOutput(inputLines); // block until done.
    
    wsOutput.outputPerLine.get(5) match {
      case Some(Seq(actual)) => assert(actual.indexOf("5") >= 0, actual);
      case None => fail("No output was given.");
    }
    wsOutput.outputPerLine.get(6) match {
      case Some(Seq(actual)) => assert(actual.indexOf("6") >= 0, actual);
      case None => fail("No output was given.");
    }
  }

  it should "gracefully handle segfaults" in {
    val inputProgram = """#include <stdio.h>

int main(int argc, char* argv) { // Line 03
  int *p = 0; // So we can throw SIGSEGV at runtime.
  *p = 5;
}""";
    val inputLines = inputProgram.lines.toList;
    
    val wsOutput = new WorksheetOutput();
    Worksheetify.processWorksheet(inputLines, wsOutput);
    val wsOutputStr = wsOutput.generateWorksheetOutput(inputLines); // block until done.
    
    wsOutput.outputPerLine.get(5) match {
      case Some(Seq(actual)) => assert(actual.toLowerCase().indexOf("seg") >= 0, actual);
      case None => fail("No output was given.");
    }
  }
}