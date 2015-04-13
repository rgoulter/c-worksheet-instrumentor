package edu.nus.worksheet.instrumentor.test

import org.scalatest._
import edu.nus.worksheet._
import edu.nus.worksheet.instrumentor._

class InstrumentorSpec extends FlatSpec {
  def assertProgramInstrumentsWithoutErrorsOrWarnings(inputProgram : String) {
    val instrumentedProgram = Instrumentor.instrument(inputProgram);

    val prog = new CProgram(instrumentedProgram);

    val (warnings, errors) = prog.compile();

    assert(warnings.isEmpty, "No warnings");
    assert(errors.isEmpty, "No warnings");
  }

  "Instrumentor" should "not produce warnings when instrumenting (assignment)" in {
    // Bug was that would get warnings
    // introduced for instrumenting assignments.
    val inputProgram = """#include <stdio.h>

int main(int argc, char* argv) { // Line 03
  int x;
  x = 3;
}""";
    assertProgramInstrumentsWithoutErrorsOrWarnings(inputProgram);
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
    assertProgramInstrumentsWithoutErrorsOrWarnings(inputProgram);
  }

  // Can ignore this for now.
  ignore should "not produce warnings when instrumenting, with obsolete struct initializer" in {
    val inputProgram = """#include <stdio.h>

int main(int argc, char** argv) { // Line 03
  union MyUnion { int i; float f; };
  union MyUnion u = { i: 3 };
}""";
    assertProgramInstrumentsWithoutErrorsOrWarnings(inputProgram);
  }

  it should "not produce warnings when instrumenting, with designated initializer" in {
    // Incredibly, this is distinct from the above test case.
    val inputProgram = """#include <stdio.h>

int main(int argc, char** argv) { // Line 03
  union MyUnion { int i; float f; };
  union MyUnion u = { .i = 3 };
}""";
    assertProgramInstrumentsWithoutErrorsOrWarnings(inputProgram);
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
    assertProgramInstrumentsWithoutErrorsOrWarnings(inputProgram);
  }

  it should "not produce errors when instrumenting (w/ *args[])" in {
    // Bug was that would get warnings
    // introduced for instrumenting assignments.
    val inputProgram = """#include <stdio.h>
int main(int argc, char *argv[]) {
}""";
    assertProgramInstrumentsWithoutErrorsOrWarnings(inputProgram);
  }

  it should "not produce warnings when instrumenting (expression statements)" in {
    // Bug was that would get warnings
    // introduced for instrumenting assignments.
    val inputProgram = """#include <stdio.h>

int main(int argc, char* argv) { // Line 03
  5;
}""";
    assertProgramInstrumentsWithoutErrorsOrWarnings(inputProgram);
  }

  it should "not produce warnings when instrumenting (expression statements, w/ variable)" in {
    // Bug was that would get warnings
    // introduced for instrumenting assignments.
    val inputProgram = """#include <stdio.h>

int main(int argc, char* argv) { // Line 03
  int x = 5;
  x;
}""";
    assertProgramInstrumentsWithoutErrorsOrWarnings(inputProgram);
  }

  it should "not produce warnings when instrumenting (expression statements, w/ array)" in {
    // Bug was that would get warnings
    // introduced for instrumenting assignments.
    val inputProgram = """#include <stdio.h>

int main(int argc, char* argv) { // Line 03
  int arr[3] = {4, 6, 3};
  arr;
}""";
    assertProgramInstrumentsWithoutErrorsOrWarnings(inputProgram);
  }

  it should "not produce warnings when instrumenting (postfix array expression statements)" in {
    // Bug was that would get warnings
    // introduced for instrumenting assignments.
    val inputProgram = """#include <stdio.h>

int main(int argc, char* argv) { // Line 03
  int arr[3] = {3,4,5};

  // Various postfix expressions
  arr[1];
}""";
    assertProgramInstrumentsWithoutErrorsOrWarnings(inputProgram);
  }

  it should "not produce warnings when instrumenting (postfix fn call expression statements)" in {
    // Bug was that would get warnings
    // introduced for instrumenting assignments.
    val inputProgram = """#include <stdio.h>

int f() {
  return 5;
}

int main(int argc, char* argv) { // Line 03
  // Various postfix expressions
  f();
}""";
    assertProgramInstrumentsWithoutErrorsOrWarnings(inputProgram);
  }

  it should "not produce warnings when instrumenting (postfix struct expression statements)" in {
    // Bug was that would get warnings
    // introduced for instrumenting assignments.
    val inputProgram = """#include <stdio.h>

int main(int argc, char* argv) { // Line 03
  struct S {int x; int y;} s1 = {3, 4};
  struct S *ps1 = &s1;

  // Various postfix expressions
  s1.x;
  ps1->y;
}""";
    assertProgramInstrumentsWithoutErrorsOrWarnings(inputProgram);
  }

  ignore should "not produce warnings when instrumenting (postfix incr/decr expression statements)" in {
    // Bug was that would get warnings
    // introduced for instrumenting assignments.
    val inputProgram = """#include <stdio.h>

int main(int argc, char* argv) { // Line 03
  int i = 10;

  // Various postfix expressions
  i++;
  i++;
  i--;
}""";
    assertProgramInstrumentsWithoutErrorsOrWarnings(inputProgram);
  }

  it should "not produce warnings when instrumenting (postfix compound literal expression statements)" in {
    // Bug was that would get warnings
    // introduced for instrumenting assignments.
    val inputProgram = """#include <stdio.h>

int main(int argc, char* argv) { // Line 03
  // Various postfix expressions
  (int[3]) {7,6,8};
}""";
    assertProgramInstrumentsWithoutErrorsOrWarnings(inputProgram);
  }

  it should "be able to instrument an array declaration where leftmost dimension not specified in arr decl." in {
    val inputProgram = """#include <stdio.h>
int main(int argc, char **argv) { // Line 02
  int (*p)[3];
  int a[] = {1, 2, 3};
  p = &a;
  printf("%d\n", a[2]);
}""";
    assertProgramInstrumentsWithoutErrorsOrWarnings(inputProgram);
  }

  it should "be able to instrument a ptr-to-array declaration where leftmost dimension not specified (with dignity)" in {
    val inputProgram = """#include <stdio.h>
int main(int argc, char **argv) { // Line 02
  int (*p)[];
  int a[] = {1, 2, 3};
  p = &a;
  printf("%d\n", a[2]);
}""";
    assertProgramInstrumentsWithoutErrorsOrWarnings(inputProgram);
  }
  it should "correctly instrument one-liner functions" in {
    val inputProgram = """#include <stdio.h>
int main(int argc, char *argv[]) { printf("Hello World\n"); }""";
    assertProgramInstrumentsWithoutErrorsOrWarnings(inputProgram);
  }

  it should "correctly instrument non-trivial one-liner functions" in {
    val inputProgram = """#include <stdio.h>
int main(int argc, char *argv[]) { int x; x = 5; printf("%d\n", x); }""";
    assertProgramInstrumentsWithoutErrorsOrWarnings(inputProgram);
  }

  it should "be able to instrument a program with function-pointers" in {
    val inputProgram = """#include <stdio.h>

int f1(int x) {
    return x + 1;
}

int main(int argc, char **argv) {
    int (*fp)(int);
    fp = f1;
    int res = fp(5);
    printf("%d\n", res);
}""";
    assertProgramInstrumentsWithoutErrorsOrWarnings(inputProgram);
  }

  it should "be able to instrument a program with for loops" in {
    val inputProgram = """#include <stdio.h>

int main(int argc, char **argv) {
    for (int i = 0; i < 5; i++) {
        printf("Hello\n");
    }
}""";
    assertProgramInstrumentsWithoutErrorsOrWarnings(inputProgram);
  }
}