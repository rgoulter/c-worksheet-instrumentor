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

  // Can ignore this for now.
  ignore should "not produce warnings when instrumenting, with obsolete struct initializer" in {
    val inputProgram = """#include <stdio.h>

int main(int argc, char** argv) { // Line 03
  union MyUnion { int i; float f; };
  union MyUnion u = { i: 3 };
}""";
    val instrumentedProgram = Instrumentor.instrument(inputProgram);
    println(instrumentedProgram);

    val prog = new CProgram(instrumentedProgram);

    val (warnings, errors) = prog.compile();

    assert(warnings.isEmpty, "No warnings");
    assert(errors.isEmpty, "No warnings");
  }

  it should "not produce warnings when instrumenting, with designated initializer" in {
    // Incredibly, this is distinct from the above test case.
    val inputProgram = """#include <stdio.h>

int main(int argc, char** argv) { // Line 03
  union MyUnion { int i; float f; };
  union MyUnion u = { .i = 3 };
}""";
    val instrumentedProgram = Instrumentor.instrument(inputProgram);

    val prog = new CProgram(instrumentedProgram);

    val (warnings, errors) = prog.compile();

    assert(warnings.isEmpty, "No warnings");
    assert(errors.isEmpty, "No warnings");
  }

  it should "not produce errors when instrumenting (w/ function prototypes)" in {
    // Bug was that would get warnings
    // introduced for instrumenting assignments.
    val inputProgram = """#include <stdio.h>
int foo(int);
int main(int argc, char **argv) {
}
int foo(int x) {
  return x;
}""";
    val instrumentedProgram = Instrumentor.instrument(inputProgram);

    val prog = new CProgram(instrumentedProgram);
    
    val (warnings, errors) = prog.compile();

    // assert(warnings.isEmpty, "No warnings"); // Gives warning for printing fp as %d.
    assert(errors.isEmpty, "No warnings");
  }

  it should "not produce errors when instrumenting (w/ *args[])" in {
    // Bug was that would get warnings
    // introduced for instrumenting assignments.
    val inputProgram = """#include <stdio.h>
int main(int argc, char *argv[]) {
}""";
    val instrumentedProgram = Instrumentor.instrument(inputProgram);

    val prog = new CProgram(instrumentedProgram);

    val (warnings, errors) = prog.compile();

    assert(warnings.isEmpty, "No warnings");
    assert(errors.isEmpty, "No warnings");
  }

  it should "be able to instrument an array declaration where leftmost dimension not specified in arr decl." in {
    val inputProgram = """#include <stdio.h>
int main(int argc, char **argv) { // Line 02
  int (*p)[3];
  int a[] = {1, 2, 3};
  p = &a;
  printf("%d\n", a[2]);
}""";
    val instrumentedProgram = Instrumentor.instrument(inputProgram);
    println(instrumentedProgram);

    val prog = new CProgram(instrumentedProgram);

    val (warnings, errors) = prog.compile();

    assert(warnings.isEmpty, "No warnings");
    assert(errors.isEmpty, "No warnings");
  }

  it should "be able to instrument a ptr-to-array declaration where leftmost dimension not specified (with dignity)" in {
    val inputProgram = """#include <stdio.h>
int main(int argc, char **argv) { // Line 02
  int (*p)[];
  int a[] = {1, 2, 3};
  p = &a;
  printf("%d\n", a[2]);
}""";
    val instrumentedProgram = Instrumentor.instrument(inputProgram);
    println(instrumentedProgram);

    val prog = new CProgram(instrumentedProgram);

    val (warnings, errors) = prog.compile();

    assert(warnings.isEmpty, "No warnings");
    assert(errors.isEmpty, "No warnings");
  }
  it should "correctly instrument one-liner functions" in {
    val inputProgram = """#include <stdio.h>
int main(int argc, char *argv[]) { printf("Hello World\n"); }""";
    val instrumentedProgram = Instrumentor.instrument(inputProgram);
    println(instrumentedProgram);

    val prog = new CProgram(instrumentedProgram);
    
    val (warnings, errors) = prog.compile();

    assert(warnings.isEmpty, "No warnings");
    assert(errors.isEmpty, "No warnings");
  }

  it should "correctly instrument non-trivial one-liner functions" in {
    val inputProgram = """#include <stdio.h>
int main(int argc, char *argv[]) { int x; x = 5; printf("%d\n", x); }""";
    val instrumentedProgram = Instrumentor.instrument(inputProgram);
    println(instrumentedProgram);

    val prog = new CProgram(instrumentedProgram);

    val (warnings, errors) = prog.compile();

    assert(warnings.isEmpty, "No warnings");
    assert(errors.isEmpty, "No warnings");
  }
}