package edu.nus.worksheet.instrumentor.test

import org.scalatest._
import edu.nus.worksheet._
import edu.nus.worksheet.instrumentor._

class InstrumentorSpec extends FlatSpec {

  "Instrumentor" should "not produce warnings when instrumenting (assignment)" in {
    // Bug was that would get warnings
    // introduced for instrumenting assignments.
    val inputProgram = """#include <stdio.h>

int main(int argc, char* argv) { // Line 03
  int x;
  x = 3;
}""";
    val instrumentedProgram = Instrumentor.instrument(inputProgram);

    val prog = new CProgram(instrumentedProgram);
    
    val (warnings, errors) = prog.compile();

    assert(warnings.isEmpty, "No warnings");
    assert(errors.isEmpty, "No warnings");
  }

  it should "not produce warnings when instrumenting (struct assignment)" in {
    // Bug was that would get warnings
    // introduced for instrumenting assignments.
    val inputProgram = """#include <stdio.h>

int main(int argc, char* argv) { // Line 03
  struct S { int x; };
  struct S s1 = {0};
  struct S s2 = {5};
  s1 = s2;
}""";
    val instrumentedProgram = Instrumentor.instrument(inputProgram);

    val prog = new CProgram(instrumentedProgram);
    
    val (warnings, errors) = prog.compile();

    assert(warnings.isEmpty, "No warnings");
    assert(errors.isEmpty, "No warnings");
  }
}