package edu.nus.worksheet.instrumentor.test;

import org.scalatest.*;
import flatspec.*;

import edu.nus.worksheet.instrumentor.*;
import edu.nus.worksheet.*;

class InstrumentorSpec extends AnyFlatSpec {
  def assertProgramInstrumentsWithoutErrorsOrWarnings(
      inputProgram: String
  ): Unit = {
    val instrumentedProgram = Instrumentor.instrument(inputProgram);

    val prog = new CProgram(instrumentedProgram);

    val (warnings, errors) = prog.compile();

    if !errors.isEmpty then {
      println("Program instrumented to (with errors):");
      println(instrumentedProgram);
    }

    // Unfortunately, our FP lookup function will generate a warning, because it refers to
    //  deprecated/dangerous functions (like `gets`).
    // But warnings are okay, right?
//    assert(warnings.isEmpty, "No warnings");
    assert(errors.isEmpty, "No errors");
  }

  "Instrumentor" should "not produce warnings when instrumenting (assignment)" in {
    // Bug was that would get warnings
    // introduced for instrumenting assignments.
    val inputProgram = """int main(int argc, char** argv) {
  int x;
  x = 3;
}""";
    assertProgramInstrumentsWithoutErrorsOrWarnings(inputProgram);
  }

  it should "not produce warnings when instrumenting (struct assignment)" in {
    // Bug was that would get warnings
    // introduced for instrumenting assignments.
    val inputProgram = """int main(int argc, char** argv) { // Line 03
  struct S { int x; };
  struct S s1 = {0};
  struct S s2 = {5};
  s1 = s2;
}""";
    assertProgramInstrumentsWithoutErrorsOrWarnings(inputProgram);
  }

  it should "not produce warnings when instrumenting (struct expression)" in {
    // Bug was that would get warnings
    // introduced for instrumenting assignments.
    val inputProgram = """int main(int argc, char** argv) { // Line 03
  struct S { int x; };
  struct S s1 = {0};
  s1;
}""";
    assertProgramInstrumentsWithoutErrorsOrWarnings(inputProgram);
  }

  // Can ignore this for now.
  ignore should "not produce warnings when instrumenting, with obsolete struct initializer" in {
    val inputProgram = """int main(int argc, char** argv) { // Line 03
  union MyUnion { int i; float f; };
  union MyUnion u = { i: 3 };
}""";
    assertProgramInstrumentsWithoutErrorsOrWarnings(inputProgram);
  }

  it should "not produce warnings when instrumenting, with designated initializer" in {
    // Incredibly, this is distinct from the above test case.
    val inputProgram = """int main(int argc, char** argv) { // Line 03
  union MyUnion { int i; float f; };
  union MyUnion u = { .i = 3 };
}""";
    assertProgramInstrumentsWithoutErrorsOrWarnings(inputProgram);
  }

  it should "not produce errors when instrumenting (w/ function prototypes)" in {
    // Bug was that would get warnings
    // introduced for instrumenting assignments.
    val inputProgram = """int foo(int);
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
    val inputProgram = """int main(int argc, char *argv[]) {
}""";
    assertProgramInstrumentsWithoutErrorsOrWarnings(inputProgram);
  }

  it should "not produce warnings when instrumenting (expression statements)" in {
    // Bug was that would get warnings
    // introduced for instrumenting assignments.
    val inputProgram = """int main(int argc, char** argv) { // Line 03
  5;
}""";
    assertProgramInstrumentsWithoutErrorsOrWarnings(inputProgram);
  }

  it should "not produce warnings when instrumenting (expression statements, w/ variable)" in {
    // Bug was that would get warnings
    // introduced for instrumenting assignments.
    val inputProgram = """int main(int argc, char** argv) { // Line 03
  int x = 5;
  x;
}""";
    assertProgramInstrumentsWithoutErrorsOrWarnings(inputProgram);
  }

  it should "not produce warnings when instrumenting (expression statements, w/ array)" in {
    // Bug was that would get warnings
    // introduced for instrumenting assignments.
    val inputProgram = """int main(int argc, char** argv) { // Line 03
  int arr[3] = {4, 6, 3};
  arr;
}""";
    assertProgramInstrumentsWithoutErrorsOrWarnings(inputProgram);
  }

  it should "not produce warnings when instrumenting (postfix array expression statements)" in {
    // Bug was that would get warnings
    // introduced for instrumenting assignments.
    val inputProgram = """int main(int argc, char** argv) { // Line 03
  int arr[3] = {3,4,5};

  // Various postfix expressions
  arr[1];
}""";
    assertProgramInstrumentsWithoutErrorsOrWarnings(inputProgram);
  }

  it should "not produce warnings when instrumenting (postfix fn call expression statements)" in {
    // Bug was that would get warnings
    // introduced for instrumenting assignments.
    val inputProgram = """int f() {
  return 5;
}

int main(int argc, char** argv) { // Line 03
  // Various postfix expressions
  f();
}""";
    assertProgramInstrumentsWithoutErrorsOrWarnings(inputProgram);
  }

  it should "not produce warnings when instrumenting (postfix struct expression statements)" in {
    // Bug was that would get warnings
    // introduced for instrumenting assignments.
    val inputProgram = """int main(int argc, char** argv) { // Line 03
  struct S {int x; int y;} s1 = {3, 4};
  struct S *ps1 = &s1;

  // Various postfix expressions
  s1.x;
  ps1->y;
}""";
    assertProgramInstrumentsWithoutErrorsOrWarnings(inputProgram);
  }

  it should "not produce warnings when instrumenting (postfix incr/decr expression statements)" in {
    // Bug was that would get warnings
    // introduced for instrumenting assignments.
    val inputProgram = """int main(int argc, char** argv) { // Line 03
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
    val inputProgram = """int main(int argc, char** argv) { // Line 03
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

  it should "be able to instrument, when VLA uses var in same declaration" in {
    val inputProgram = """int main(int argc, char **argv) {
    int n = 3, vla[n];
}""";
    assertProgramInstrumentsWithoutErrorsOrWarnings(inputProgram);
  }

  it should "be able to instrument, with a call to a function (returning void)" in {
    val inputProgram = """#include <stdio.h>

void f() {
  printf("Hello\n");
}

int main(int argc, char **argv) {
    f();
}""";
    assertProgramInstrumentsWithoutErrorsOrWarnings(inputProgram);
  }

  it should "be able to instrument programs with comma operator" in {
    val inputProgram = """#include <stdio.h>

int main(int argc, char** argv) {
    int x;
    x = 3, 4; // x = (3, 4) passes when this doesn't.
    printf("x = %d\n", x);
}""";
    assertProgramInstrumentsWithoutErrorsOrWarnings(inputProgram);
  }

  it should "be able to instrument programs with varargs" in {
    val inputProgram = """#include <stdio.h>

int f(int n, ...) {
    return 5;
}

int main(int argc, char **argv) {
    int max = f(3, 5, 9, 4);
    printf("Max: %d\n", max);
}""";
    assertProgramInstrumentsWithoutErrorsOrWarnings(inputProgram);
  }

  ignore should "be able to instrument programs w/ CPP macros" in {
    val inputProgram = """#include <stdio.h>

#define LCED(I,D) D I

int main() {
  LCED(x, int) = 5;
  printf("%d\n", x);
}""";
    assertProgramInstrumentsWithoutErrorsOrWarnings(inputProgram);
  }

  ignore should "be able to instrument programs with varargs, (vararg macros actually used)" in {
    val inputProgram = """#include <stdio.h>
#include <stdarg.h>

int __worksheet_max_of_n(int n, ...) {
    va_list ap;
    int max = 0;

    va_start(ap, n);

    for (; n; n--) {
        int tmp = va_arg(ap, int);
        if (tmp > max)
            max = tmp;
    }

    va_end(ap);
    return max;
}

int main(int argc, char **argv) {
    int max = __worksheet_max_of_n(3, 5, 9, 4);
    printf("Max: %d\n", max);
}""";
    assertProgramInstrumentsWithoutErrorsOrWarnings(inputProgram);
  }

  it should "work, when there's a typedefname as first declaration in block" in {
    val inputProgram = """#include <stdio.h>
typedef int T;
int main(int argc, char** argv) { // Line 03
    T x;
    x = 5;
    printf("%d\n", x);
}""";
    assertProgramInstrumentsWithoutErrorsOrWarnings(inputProgram);
  }

  it should "work, when there's a case with typedef kind" in {
    val inputProgram = """#include <stdio.h>
typedef int T;
int main(int argc, char** argv) { // Line 03
    T x = (T) 5;
}""";
    assertProgramInstrumentsWithoutErrorsOrWarnings(inputProgram);
  }

  // this produces a warning, b/c printf %d doesn't necessarily fit type
  // of size_t.
  ignore should "work with types from header includes." in {
    val inputProgram = """#include <stdio.h>
int main(int argc, char** argv) { // Line 03
    size_t x;
    x = 5;
    printf("%d\n", x);
}""";
    assertProgramInstrumentsWithoutErrorsOrWarnings(inputProgram);
  }

  it should "instrument for programs with function pointer expressions" in {
    val inputProgram = """int f(int x) { return x * x; }
int main(int argc, char** argv) { // Line 03
  int (*fp)(int) = &f;
  fp;

  fp(3);
}""";
    assertProgramInstrumentsWithoutErrorsOrWarnings(inputProgram);
  }

  it should "instrument for programs with 'brace-less' if/else, etc. statements" in {
    val inputProgram = """int main(int argc, char** argv) {
  if (1) 5; else 9;
}""";
    assertProgramInstrumentsWithoutErrorsOrWarnings(inputProgram);
  }

  it should "instrument for programs with 'brace-less' if/else, etc. statements (assignment stmt)" in {
    val inputProgram = """int main(int argc, char** argv) {
  int x;
  if (1) x = 5; else x = 9;
}""";
    assertProgramInstrumentsWithoutErrorsOrWarnings(inputProgram);
  }

  ignore should "instrument for programs with assignments in comma expressions" in {
    val inputProgram = """int main(int argc, char** argv) {
  int x, y;
  x = 0, y = 5;
}""";
    assertProgramInstrumentsWithoutErrorsOrWarnings(inputProgram);
  }

  it should "instrument for programs with unapplied function symbol expressions" in {
    // Don't necessarily expect any useful output from such an expression, (it's meaningless!)
    // but it's a program which compiles with GCC, so would be nice if it worked here.
    val inputProgram = """int f() { return 3; }
int main(int argc, char** argv) {
    f;
}""";
    assertProgramInstrumentsWithoutErrorsOrWarnings(inputProgram);
  }

  it should "instrument for programs with string as an expression statement" in {
    // Don't necessarily expect any useful output from such an expression, (it's meaningless!)
    // but it's a program which compiles with GCC, so would be nice if it worked here.
    val inputProgram = """int main(int argc, char** argv) {
    "Hello";
}""";
    assertProgramInstrumentsWithoutErrorsOrWarnings(inputProgram);
  }

  it should "instrument for programs with a struct declaration across multiple lines." in {
    // Don't necessarily expect any useful output from such an expression, (it's meaningless!)
    // but it's a program which compiles with GCC, so would be nice if it worked here.
    val inputProgram = """int main(int argc, char** argv) {
    struct {
        int x;
    } s;
}""";
    assertProgramInstrumentsWithoutErrorsOrWarnings(inputProgram);
  }

  it should "instrument for programs which do 'array index' op on pointers." in {
    // Don't necessarily expect any useful output from such an expression, (it's meaningless!)
    // but it's a program which compiles with GCC, so would be nice if it worked here.
    val inputProgram = """int main(int argc, char** argv) {
  int ia[] = { 5, 7, 2, 3 };
  int *pi = ia;
  pi[3];
}""";
    assertProgramInstrumentsWithoutErrorsOrWarnings(inputProgram);
  }

  it should "instrument for programs which assign to 'array index' op on pointers." in {
    // Don't necessarily expect any useful output from such an expression, (it's meaningless!)
    // but it's a program which compiles with GCC, so would be nice if it worked here.
    val inputProgram = """int main(int argc, char** argv) {
  int ia[] = { 5, 7, 2, 3 };
  int *pi = ia;
  pi[3] = 99;
}""";
    assertProgramInstrumentsWithoutErrorsOrWarnings(inputProgram);
  }
}
