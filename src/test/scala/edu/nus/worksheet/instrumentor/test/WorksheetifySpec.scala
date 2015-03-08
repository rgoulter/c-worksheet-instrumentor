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

  it should "output printf on correct line, with function calls as parameters." in {
    val inputProgram = """#include <stdio.h>
int foo() {
  return 1;
}

int main(int argc, char* argv) { // Line 06
  printf("%d\n", foo());
}""";
    val inputLines = inputProgram.lines.toList;
    
    val wsOutput = new WorksheetOutput();
    Worksheetify.processWorksheet(inputLines, wsOutput);
    val wsOutputStr = wsOutput.generateWorksheetOutput(inputLines); // block until done.
    
    wsOutput.outputPerLine.get(7) match {
      case Some(actual) => assert(actual.length > 0);
      case None => fail("No output was given.");
    }
  }

  it should "output printf on correct line, even without a newline." in {
    val inputProgram = """#include <stdio.h>

int main(int argc, char* argv) { // Line 03
  printf("a");
  printf("b\n");
}""";
    val inputLines = inputProgram.lines.toList;
    
    val wsOutput = new WorksheetOutput();
    Worksheetify.processWorksheet(inputLines, wsOutput);
    val wsOutputStr = wsOutput.generateWorksheetOutput(inputLines); // block until done.
    
    // If this test is broken, it'll look like line 04 has output "aLINE5\nb"
    // and so line 5 won't have output.
    wsOutput.outputPerLine.get(5) match {
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

  it should "be able to dereference a valid pointer" in {
    val inputProgram = """#include <stdio.h>

int main(int argc, char* argv) { // Line 03
  int x = 5;
  int *p;
  p = &x;
}""";
    val inputLines = inputProgram.lines.toList;
    
    val wsOutput = new WorksheetOutput();
    Worksheetify.processWorksheet(inputLines, wsOutput);
    val wsOutputStr = wsOutput.generateWorksheetOutput(inputLines); // block until done.

    wsOutput.outputPerLine.get(6) match {
      case Some(Seq(actual)) => assert(actual.charAt(actual.length() - 1) == '5', actual);
      case None => fail("No output was given.");
    }
  }

  it should "be able to dereference an invalid pointer" in {
    val inputProgram = """#include <stdio.h>

int main(int argc, char* argv) { // Line 03
  int x = 5;
  int *p;
  p = 0;
}""";
    val inputLines = inputProgram.lines.toList;
    
    val wsOutput = new WorksheetOutput();
    Worksheetify.processWorksheet(inputLines, wsOutput);
    val wsOutputStr = wsOutput.generateWorksheetOutput(inputLines); // block until done.

    wsOutput.outputPerLine.get(6) match {
      // If we couldn't dereference pointers, we'd get a segfault.
      case Some(Seq(actual)) => assert(actual.toLowerCase().indexOf("seg") < 0, actual);
      case None => fail("No output was given.");
    }
  }

  it should "output member ids in structs/unions" in {
    val inputProgram = """#include <stdio.h>

int main(int argc, char* argv) { // Line 03
  union MyU { int unInt; float unFloat; };
  union MyU u = {.unInt = 5};
  u = u;
}""";
    val inputLines = inputProgram.lines.toList;
    
    val wsOutput = new WorksheetOutput();
    Worksheetify.processWorksheet(inputLines, wsOutput);
    val wsOutputStr = wsOutput.generateWorksheetOutput(inputLines); // block until done.

    wsOutput.outputPerLine.get(6) match {
      // If we couldn't dereference pointers, we'd get a segfault.
      case Some(Seq(actual)) => {
        assert(actual.indexOf("unInt") >= 0, actual);
        assert(actual.indexOf("unFloat") >= 0, actual);
      }
      case None => fail("No output was given.");
    }
  }

  it should "output enums." in {
    val inputProgram = """#include <stdio.h>

int main(int argc, char* argv) { // Line 03
  enum MyEnum { FOO, BAR };
  enum MyEnum e;
  e = FOO;
}""";
    val inputLines = inputProgram.lines.toList;
    
    val wsOutput = new WorksheetOutput();
    Worksheetify.processWorksheet(inputLines, wsOutput);
    val wsOutputStr = wsOutput.generateWorksheetOutput(inputLines); // block until done.

    wsOutput.outputPerLine.get(6) match {
      case Some(Seq(actual)) => assert(actual.indexOf("FOO") >= 0, actual);
      case None => fail("No output was given.");
    }
  }

  it should "interact nicely with Stdin Markup." in {
    val inputProgram = """#include <stdio.h>

//IN:
// 5
int main(int argc, char* argv) { // Line 05
  int x;
  scanf("%d", &x);
  printf("you entered %d\n", x);
}""";
    val inputLines = inputProgram.lines.toList;

    val wsOutput = new WorksheetOutput();
    val stdInput = StdinMarkup.extractFromSource(inputProgram)
    Worksheetify.processWorksheet(inputLines, wsOutput, stdinLines = stdInput);
    val wsOutputStr = wsOutput.generateWorksheetOutput(inputLines); // block until done.

    wsOutput.outputPerLine.get(8) match {
      case Some(Seq(actual)) => assert(actual.indexOf("5") >= 0, actual);
      case None => fail("No output was given.");
    }
  }
}