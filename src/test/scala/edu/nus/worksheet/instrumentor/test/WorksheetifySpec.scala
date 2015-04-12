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
int foo(void) {
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

  it should "handle scopes correctly, so same name different type is okay." in {
    val inputProgram = """#include <stdio.h>
int main(int argc, char* argv) { // Line 02
  char x;
  x = 'a';
  {
    int x;
    x = 65;
  }
}""";
    val inputLines = inputProgram.lines.toList;

    val wsOutput = new WorksheetOutput();
    Worksheetify.processWorksheet(inputLines, wsOutput);
    val wsOutputStr = wsOutput.generateWorksheetOutput(inputLines); // block until done.

    wsOutput.outputPerLine.get(4) match {
      case Some(Seq(x)) => assert(x.contains("a"));
      case None => fail("No output was given.");
    }
    wsOutput.outputPerLine.get(7) match {
      case Some(Seq(x)) => assert(x.contains("65"));
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

  it should "be able to output for recursive functions" in {
    val inputProgram = """#include <stdio.h>

int fib(int n) {  // Line 03
  if (n < 2) {
    return n;
  } else {        // Line 06
    int res, f1, f2;
    f1 = fib(n - 1);
    f2 = fib(n - 2);
    res = f1 + f2;
    return res;
  }
}

int main(int argc, char* argv) { // Line 03
  int f = fib(3);
}""";
    val inputLines = inputProgram.lines.toList;

    val wsOutput = new WorksheetOutput();
    Worksheetify.processWorksheet(inputLines, wsOutput);
    val wsOutputStr = wsOutput.generateWorksheetOutput(inputLines); // block until done.

    wsOutput.outputPerLine.get(8) match {
      case Some(xs) => assertResult(2)(xs.length);
      case None => fail("No output was given.");
    }
    wsOutput.outputPerLine.get(9) match {
      case Some(xs) => assertResult(2)(xs.length);
      case None => fail("No output was given.");
    }
    wsOutput.outputPerLine.get(10) match {
      case Some(xs) => assertResult(2)(xs.length);
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

  it should "be able to instrument an array declaration where leftmost dimension of array not specified in arr decl." in {
    val inputProgram = """#include <stdio.h>
int main(int argc, char **argv) { // Line 02
  int (*p)[3];
  int a[] = {1, 2, 3};
  p = &a;
  printf("%d\n", a[2]);
}""";
    val instrumentedProgram = Instrumentor.instrument(inputProgram);

    val inputLines = inputProgram.lines.toList;

    val wsOutput = new WorksheetOutput();
    Worksheetify.processWorksheet(inputLines, wsOutput);
    val wsOutputStr = wsOutput.generateWorksheetOutput(inputLines); // block until done.

    wsOutput.outputPerLine.get(5) match {
      case Some(Seq(actual)) => assert(actual.contains("1, 2, 3"), actual);
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

  it should "output for struct in this case." in {
    val inputProgram = """#include <stdio.h>

int main(int argc, char* argv) { // Line 03
  struct S { int i; float f; };
  struct S s1 = { 2, 1.0f }, s2;
  s2 = s1;
}""";
    val inputLines = inputProgram.lines.toList;

    val wsOutput = new WorksheetOutput();
    Worksheetify.processWorksheet(inputLines, wsOutput);
    val wsOutputStr = wsOutput.generateWorksheetOutput(inputLines); // block until done.

    wsOutput.outputPerLine.get(6) match {
      // If we couldn't dereference pointers, we'd get a segfault.
      case Some(Seq(actual)) => {
        assert(actual.indexOf("i") >= 0, actual);
        assert(actual.indexOf("f") >= 0, actual);
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

  it should "not suffer interference from a worksheet printing directives" in {
    val inputProgram = """#include <stdio.h>

int main(int argc, char* argv) { // Line 03
  printf("LINE 3\nfoo");
}""";
    val inputLines = inputProgram.lines.toList;

    val wsOutput = new WorksheetOutput();
    Worksheetify.processWorksheet(inputLines, wsOutput);
    val wsOutputStr = wsOutput.generateWorksheetOutput(inputLines); // block until done.

    // If this test is broken, it'll look like line 04 has output "aLINE5\nb"
    // and so line 5 won't have output.
    wsOutput.outputPerLine.get(3) match {
      case Some(actual) => fail("Should not contain output.");
      case None => ();
    }
  }
}