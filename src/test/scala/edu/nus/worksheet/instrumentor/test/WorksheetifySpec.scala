package edu.nus.worksheet.instrumentor.test

import java.util.Timer
import java.util.TimerTask
import org.scalatest._
import flatspec._
import edu.nus.worksheet._
import edu.nus.worksheet.instrumentor._

class WorksheetifySpec extends AnyFlatSpec {

  "Worksheetify" should "output info for declarations" in {
    val inputProgram = """#include <stdio.h>

int main(int argc, char **argv) { // Line 03
  int x;
}""";

    val wsOutput = Worksheetify.worksheetifyForInput(inputProgram);
    wsOutput.generateWorksheetOutput(); // block until WS done.

    wsOutput.outputPerLine.get(4) match {
      case Some(actual) => assert(actual.length > 0);
      case None => fail("No output was given.");
    }
  }

  it should "not output info for the same declaration multiple times" in {
    val inputProgram = """#include <stdio.h>

int f() { // Line 03
  int x;
  x = 5;
  return x;
}

int main(int argc, char **argv) { // Line 09
  f();
  f();
}""";

    val wsOutput = Worksheetify.worksheetifyForInput(inputProgram);
    wsOutput.generateWorksheetOutput(); // block until WS done.

    wsOutput.outputPerLine.get(4) match {
      case Some(s) => assertResult(1, "Should only be declared once")(s.length);
      case None => fail("No output was given.");
    }
  }

  it should "output printf on correct line, with function calls as parameters." in {
    val inputProgram = """#include <stdio.h>
int foo(void) {
  return 1;
}

int main(int argc, char **argv) { // Line 06
  printf("%d\n", foo());
}""";

    val wsOutput = Worksheetify.worksheetifyForInput(inputProgram);
    wsOutput.generateWorksheetOutput(); // block until WS done.

    wsOutput.outputPerLine.get(7) match {
      case Some(actual) => assert(actual.length > 0);
      case None => fail("No output was given.");
    }
  }

  it should "output printf on correct line, even without a newline." in {
    val inputProgram = """#include <stdio.h>

int main(int argc, char **argv) { // Line 03
  printf("a");
  printf("b\n");
}""";

    val wsOutput = Worksheetify.worksheetifyForInput(inputProgram);
    wsOutput.generateWorksheetOutput(); // block until WS done.

    // If this test is broken, it'll look like line 04 has output "aLINE5\nb"
    // and so line 5 won't have output.
    wsOutput.outputPerLine.get(5) match {
      case Some(actual) => assert(actual.length > 0);
      case None => fail("No output was given.");
    }

    // Because `printf` returns a result,
    // need to check that no WORKSHEET directive gets output here.
    wsOutput.outputPerLine.get(4) match {
      case Some(xs) => assert(xs.length == 1 && !xs.head.contains("WORKSHEET"));
      case None => fail("No output was given.");
    }
  }

  it should "handle scopes correctly, so same name different type is okay." in {
    val inputProgram = """#include <stdio.h>
int main(int argc, char **argv) { // Line 02
  char x;
  x = 'a';
  {
    int x;
    x = 65;
  }
}""";

    val wsOutput = Worksheetify.worksheetifyForInput(inputProgram);
    wsOutput.generateWorksheetOutput(); // block until WS done.

    wsOutput.outputPerLine.get(4) match {
      case Some(Seq(x)) => assert(x.contains("a"));
      case None => fail("No output was given.");
    }
    wsOutput.outputPerLine.get(7) match {
      case Some(Seq(x)) => assert(x.contains("65"));
      case None => fail("No output was given.");
    }
  }

  it should "handle scopes correctly, with structs in different scopes." in {
    val inputProgram = """#include <stdio.h>
int main(int argc, char **argv) { // Line 02
  struct S { char * data; };
  struct S x = { "hello" };
  { // Line 05
    struct S { int data; };
    struct S x = { 5 };
    x.data;
  } // Line 09
  x.data;
}""";

    val wsOutput = Worksheetify.worksheetifyForInput(inputProgram);
    wsOutput.generateWorksheetOutput(); // block until WS done.

    wsOutput.outputPerLine.get(8) match {
      case Some(Seq(x)) => assert(x.contains("5"));
      case None => fail("No output was given.");
    }
    wsOutput.outputPerLine.get(10) match {
      case Some(Seq(x)) => assert(x.contains("hello"));
      case None => fail("No output was given.");
    }
  }

  it should "handle scopes correctly, with typedefs in different scopes." in {
    val inputProgram = """#include <stdio.h>
int main(int argc, char **argv) { // Line 02
  typedef char *T;
  T x;
  {  // Line 05
    typedef int T;
    T x;
    x = 5;
  }  // Line 09
  x = "hello";
}""";

    val wsOutput = Worksheetify.worksheetifyForInput(inputProgram);
    wsOutput.generateWorksheetOutput(); // block until WS done.

    wsOutput.outputPerLine.get(8) match {
      case Some(Seq(x)) => assert(x.contains("5"));
      case None => fail("No output was given.");
    }
    wsOutput.outputPerLine.get(10) match {
      case Some(Seq(x)) => assert(x.contains("hello"));
      case None => fail("No output was given.");
    }
  }

  it should "output info for assignments, on the correct line." in {
    val inputProgram = """#include <stdio.h>

int main(int argc, char **argv) { // Line 03
  int x;
  x = 5;
  x = 6;
}""";

    val wsOutput = Worksheetify.worksheetifyForInput(inputProgram);
    wsOutput.generateWorksheetOutput(); // block until WS done.

    wsOutput.outputPerLine.get(5) match {
      case Some(Seq(actual)) => assert(actual.indexOf("5") >= 0, actual);
      case None => fail("No output was given.");
    }
    wsOutput.outputPerLine.get(6) match {
      case Some(Seq(actual)) => assert(actual.indexOf("6") >= 0, actual);
      case None => fail("No output was given.");
    }
  }

  it should "output info for assignments, on the correct line. (in a for loop, braceless)" in {
    val inputProgram =
"""int main(int argc, char **argv) { // Line 01
  int x = 0;
  for (int i = 0; i < 5; i++)
    x += i;

  x;
}""";

    val wsOutput = Worksheetify.worksheetifyForInput(inputProgram);
    wsOutput.generateWorksheetOutput(); // block until WS done.

    wsOutput.outputPerLine.get(4) match {
      case Some(output) => assert(output.length == 5);
      case None => fail("No output was given.");
    }

    // Line 6 contains sum. Should be '10'.
    wsOutput.outputPerLine.get(6) match {
      case Some(Seq(actual)) => assert(actual.contains("10"), actual);
      case None => fail("No output was given.");
    }
  }

  it should "output info for assignments (where lvalue isn't just an identifier)" in {
    val inputProgram = """#include <stdio.h>
int main(int argc, char **argv) { // Line 02
  struct S { int x; } s = { 5 };
  s.x = 3;
  s.x;
}""";

    val wsOutput = Worksheetify.worksheetifyForInput(inputProgram);
    wsOutput.generateWorksheetOutput(); // block until WS done.

    wsOutput.outputPerLine.get(4) match {
      case Some(Seq(actual)) => assert(actual.contains("3"), actual);
      case None => fail("No output was given.");
    }
    wsOutput.outputPerLine.get(5) match {
      case Some(Seq(actual)) => assert(actual.contains("3"), actual);
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

int main(int argc, char **argv) { // Line 03
  int f = fib(3);
}""";

    val wsOutput = Worksheetify.worksheetifyForInput(inputProgram);
    wsOutput.generateWorksheetOutput(); // block until WS done.

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

int main(int argc, char **argv) { // Line 03
  int *p = 0; // So we can throw SIGSEGV at runtime.
  *p = 5;
}""";

    val wsOutput = Worksheetify.worksheetifyForInput(inputProgram);
    wsOutput.generateWorksheetOutput(); // block until WS done.

    wsOutput.outputPerLine.get(5) match {
      case Some(Seq(actual)) => assert(actual.toLowerCase().indexOf("seg") >= 0, actual);
      case None => fail("No output was given.");
    }
  }

  it should "be able to dereference a valid pointer" in {
    val inputProgram = """#include <stdio.h>

int main(int argc, char **argv) { // Line 03
  int x = 5;
  int *p;
  p = &x;
}""";

    val wsOutput = Worksheetify.worksheetifyForInput(inputProgram);
    wsOutput.generateWorksheetOutput(); // block until WS done.

    wsOutput.outputPerLine.get(6) match {
      case Some(Seq(actual)) => assert(actual.charAt(actual.length() - 1) == '5', actual);
      case None => fail("No output was given.");
    }
  }

  it should "be able to dereference an invalid pointer" in {
    val inputProgram = """#include <stdio.h>

int main(int argc, char **argv) { // Line 03
  int x = 5;
  int *p;
  p = 0;
}""";

    val wsOutput = Worksheetify.worksheetifyForInput(inputProgram);
    wsOutput.generateWorksheetOutput(); // block until WS done.

    wsOutput.outputPerLine.get(6) match {
      // If we couldn't dereference pointers, we'd get a segfault.
      case Some(Seq(actual)) => assert(actual.toLowerCase().indexOf("seg") < 0, actual);
      case None => fail("No output was given.");
    }
  }

  it should "be able to instrument an array (not in an assignment)" in {
    val inputProgram = """#include <stdio.h>
int main(int argc, char **argv) { // Line 02
  int a[3] = {1, 2, 3};
  a;
}""";

    val wsOutput = Worksheetify.worksheetifyForInput(inputProgram);
    wsOutput.generateWorksheetOutput(); // block until WS done.

    wsOutput.outputPerLine.get(4) match {
      case Some(Seq(actual)) => assert(actual.contains("1, 2, 3"), actual);
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

    val wsOutput = Worksheetify.worksheetifyForInput(inputProgram);
    wsOutput.generateWorksheetOutput(); // block until WS done.

    wsOutput.outputPerLine.get(5) match {
      case Some(Seq(actual)) => assert(actual.contains("1, 2, 3"), actual);
      case None => fail("No output was given.");
    }
  }

  it should "be able to instrument arrays with variety of initializers" in {
    val inputProgram = """#include <stdio.h>

int main(int argc, char **argv) { // line 03
  int arr1[] = { 2, 3, 4 };
  arr1;
  // line 06
  int arr2[] = { [2] = 1 }; // 0, 0, 1
  arr2;
  // line 09
  int arr3[][2] = { { 1, 2 }, { 3, 4 } };
  arr3;
  // line 12
  int arr4[5] = { [1*2] = 1 };
  arr4;
}
"""

    val wsOutput = Worksheetify.worksheetifyForInput(inputProgram);
    wsOutput.generateWorksheetOutput(); // block until WS done.

    // We test that the above works by counting the commas.
    wsOutput.outputPerLine.get(5) match {
      case Some(Seq(actual)) => assertResult(2, actual)(actual.count(_ == ','));
      case None => fail("No output was given.");
    }

    wsOutput.outputPerLine.get(8) match {
      case Some(Seq(actual)) => assertResult(2, actual)(actual.count(_ == ','));
      case None => fail("No output was given.");
    }

    wsOutput.outputPerLine.get(11) match {
      case Some(Seq(actual)) => assertResult(3, actual)(actual.count(_ == ','));
      case None => fail("No output was given.");
    }

    wsOutput.outputPerLine.get(14) match {
      case Some(Seq(actual)) => assertResult(4, actual)(actual.count(_ == ','));
      case None => fail("No output was given.");
    }
  }

  it should "output member ids in structs/unions" in {
    val inputProgram = """#include <stdio.h>

int main(int argc, char **argv) { // Line 03
  union MyU { int unInt; float unFloat; };
  union MyU u = {.unInt = 5};
  u = u;
}""";

    val wsOutput = Worksheetify.worksheetifyForInput(inputProgram);
    wsOutput.generateWorksheetOutput(); // block until WS done.

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

int main(int argc, char **argv) { // Line 03
  struct S { int i; float f; };
  struct S s1 = { 2, 1.0f }, s2;
  s2 = s1;
}""";

    val wsOutput = Worksheetify.worksheetifyForInput(inputProgram);
    wsOutput.generateWorksheetOutput(); // block until WS done.

    wsOutput.outputPerLine.get(6) match {
      // If we couldn't dereference pointers, we'd get a segfault.
      case Some(Seq(actual)) => {
        assert(actual.indexOf("i") >= 0, actual);
        assert(actual.indexOf("f") >= 0, actual);
      }
      case None => fail("No output was given.");
    }
  }

  it should "output struct expressions" in {
    val inputProgram = """#include <stdio.h>

int main(int argc, char **argv) { // Line 03
  struct S { int i; float f; };
  struct S s1 = { 2, 1.0f };
  s1;
}""";

    val wsOutput = Worksheetify.worksheetifyForInput(inputProgram);
    wsOutput.generateWorksheetOutput(); // block until WS done.

    wsOutput.outputPerLine.get(6) match {
      case Some(Seq(actual)) => {
        assert(actual.indexOf("i") >= 0, actual);
        assert(actual.indexOf("f") >= 0, actual);
      }
      case None => fail("No output was given.");
    }
  }

  it should "output enums." in {
    val inputProgram = """#include <stdio.h>

int main(int argc, char **argv) { // Line 03
  enum MyEnum { FOO, BAR };
  enum MyEnum e;
  e = FOO;
}""";

    val wsOutput = Worksheetify.worksheetifyForInput(inputProgram);
    wsOutput.generateWorksheetOutput(); // block until WS done.

    wsOutput.outputPerLine.get(6) match {
      case Some(Seq(actual)) => assert(actual.indexOf("FOO") >= 0, actual);
      case None => fail("No output was given.");
    }
  }

  it should "interact nicely with Stdin Markup." in {
    val inputProgram = """#include <stdio.h>

//IN:
// 5
int main(int argc, char **argv) { // Line 05
  int x;
  scanf("%d", &x);
  printf("you entered %d\n", x);
}""";

    val wsOutput = Worksheetify.worksheetifyForInput(inputProgram);
    wsOutput.generateWorksheetOutput(); // block until WS done.

    wsOutput.outputPerLine.get(8) match {
      case Some(Seq(actual)) => assert(actual.indexOf("5") >= 0, actual);
      case None => fail("No output was given.");
    }
  }

  it should "not suffer interference from a worksheet printing directives" in {
    val inputProgram = """#include <stdio.h>

int main(int argc, char **argv) { // Line 03
  printf("LINE 3\nfoo");
}""";

    val wsOutput = Worksheetify.worksheetifyForInput(inputProgram);
    wsOutput.generateWorksheetOutput(); // block until WS done.

    // If this test is broken, it'll look like line 04 has output "aLINE5\nb"
    // and so line 5 won't have output.
    wsOutput.outputPerLine.get(3) match {
      case Some(actual) => fail("Should not contain output.");
      case None => ();
    }
  }

  ignore should "output function name, for function pointer assignments" in {
    val inputProgram = """#include <stdio.h>

int f1(int x) { // Line 03
    return x + 1;
}

int main(int argc, char **argv) { // Line 7
    int (*fp)(int);
    fp = f1;
    int res = fp(5);
    printf("%d\n", res);
}""";

    val wsOutput = Worksheetify.worksheetifyForInput(inputProgram);
    wsOutput.generateWorksheetOutput(); // block until WS done.

    wsOutput.outputPerLine.get(9) match {
      // Actually, it outputs the pointer de-ref, not the name
      // of the function which the function pointer points-to.
      // It would be a nice enhancement to print out e.g. "f1" instead.
      case Some(Seq(actual)) => assert(actual.indexOf("fp") >= 0, actual);
      case None => fail("No output was given.");
    }
  }

  it should "output the expression value, for expression statements" in {
    val inputProgram = """#include <stdio.h>

int main(int argc, char **argv) { // Line 3
    5;
}""";

    val wsOutput = Worksheetify.worksheetifyForInput(inputProgram);
    wsOutput.generateWorksheetOutput(); // block until WS done.

    wsOutput.outputPerLine.get(4) match {
      case Some(Seq(actual)) => assert(actual.contains("5"), actual);
      case None => fail("No output was given.");
    }
  }

  it should "output the expression value, for variable expression statements" in {
    val inputProgram = """#include <stdio.h>

int main(int argc, char **argv) { // Line 3
    int x = 5;
    x;
}""";

    val wsOutput = Worksheetify.worksheetifyForInput(inputProgram);
    wsOutput.generateWorksheetOutput(); // block until WS done.

    wsOutput.outputPerLine.get(5) match {
      case Some(Seq(actual)) => assert(actual.contains("5"), actual);
      case None => fail("No output was given.");
    }
  }

  it should "behave sensibly with loops" in {
    val inputProgram = """#include <stdio.h>

int main(int argc, char **argv) { // Line 3
    for (int i = 0; i < 5; i++) {
        printf("Hello\n");
    }
}""";

    val wsOutput = Worksheetify.worksheetifyForInput(inputProgram);
    wsOutput.generateWorksheetOutput(); // block until WS done.

    // Don't want any output on for (..) {
    wsOutput.outputPerLine.get(4) match {
      case Some(_) => fail("Shouldn't have output");
      case None => ();
    }

    wsOutput.outputPerLine.get(5) match {
      case Some(_) => assert(true);
      case None => fail("No output was given.");
    }
  }

  it should "not evaluate an expression twice when visualising." in {
    val inputProgram = """#include <stdio.h>

int f() {  // Line 3
  static int x = 0;
  return x++;
}

int main(int argc, char **argv) { // Line 8
  f();
  f();
}""";

    val wsOutput = Worksheetify.worksheetifyForInput(inputProgram);
    wsOutput.generateWorksheetOutput(); // block until WS done.

    // f() evaluates as 0, 1, 2, ...
    // So, the above should evaluate as 0, 1.

    wsOutput.outputPerLine.get(9) match {
      case Some(Seq(x)) => assert(x == "0");
      case None => fail("No output was given.");
    }

    wsOutput.outputPerLine.get(10) match {
      case Some(Seq(x)) => assert(x == "1");
      case None => fail("No output was given.");
    }
  }

  it should "handle forward-declarations with pointer to forward-declared type" in {
    val inputProgram = """#include <stdio.h>
int main(int argc, char **argv) { // Line 02
  struct S;
  struct S;
  typedef struct S S;
  S *ptrToS;
  struct S { int x; };
  S myS = { 1234 };
  ptrToS = &myS;
} // line 10""";

    val wsOutput = Worksheetify.worksheetifyForInput(inputProgram);
    wsOutput.generateWorksheetOutput(); // block until WS done.

    wsOutput.outputPerLine.get(9) match {
      case Some(Seq(x)) => assert(x.contains("1234"));
      case None => fail("No output was given.");
    }
  }

  it should "sensibly 'filter' for a block-scope, including descendant block-scopes." in {
    val inputProgram = """#include <stdio.h>
int main(int argc, char **argv) {  // Line 02
  for (int i = 0; i < 3; i++) {
    // worksheet filter iteration == 1
    printf("outer %d\n", i);
    for (int j = 0; j < 2; j++) { // Line 06
      printf("inner %d %d\n", i, j);
    }
  }
}""";

    val wsOutput = Worksheetify.worksheetifyForInput(inputProgram);
    wsOutput.generateWorksheetOutput(); // block until WS done.

    // The inner loop outputs one line, runs twice per iteration of outer loop.
    // If the outer loop is filtered to 1 particular iteration, then this
    // should output only 2 lines.
    wsOutput.outputPerLine.get(7) match {
      case Some(xs) => assert(xs.length == 2);
      case None => fail("No output was given.");
    }
  }

  ignore should "sensibly output for function pointer expressions." in {
    val inputProgram = """#include <stdio.h>
int funcSquare(int x) { return x * x; }
int main(int argc, char **argv) { // Line 03
  int (*fp)(int) = &funcSquare;
  fp;

  fp(3);
}""";

    val wsOutput = Worksheetify.worksheetifyForInput(inputProgram);
    wsOutput.generateWorksheetOutput(); // block until WS done.

    // Reason this test is needed is because when I tried this, I got the result
    // `(*wsExprResult)`, which is clearly wrong.
    wsOutput.outputPerLine.get(5) match {
      case Some(Seq(line)) => assert(!line.contains("wsExprResult") && line.contains("funcSquare"));
      case None => fail("No output was given.");
    }
  }

  it should "sensibly (by default) limit the amount of output-per-line" in {
    // It's 'arbitrary' whether the 'cropping' here should be done from
    //  Worksheetify, or the 'UI' (Wsfy->String).
    // In either case, 50 lines of output for one line is excessive.

    val inputProgram = """#include <stdio.h>
int main(int argc, char **argv) { // Line 02
  for (int i = 0; i < 50; i++) {
    printf("%d\n", i);
  }
}""";

    val wsOutput = Worksheetify.worksheetifyForInput(inputProgram);
    wsOutput.generateWorksheetOutput(); // block until WS done.

    wsOutput.outputPerLine.get(4) match {
      case Some(xs) => assert(xs.length < 20);
      case None => fail("No output was given.");
    }
  }

  it should "limit the amount of output-per-line (to a customisable number)" in {
    // It's 'arbitrary' whether the 'cropping' here should be done from
    //  Worksheetify, or the 'UI' (Wsfy->String).
    // In either case, 50 lines of output for one line is excessive.

    val inputProgram = """#include <stdio.h>
int main(int argc, char **argv) {
  int i;
  for (i = 0; i <  50; i++) { // Line 04
    printf("%d\n", i);
  }
  for (i = 0; i < 150; i++) { // Line 07
    printf("%d\n", i);
  }
}""";

    val wsOutput = Worksheetify.worksheetifyForInput(inputProgram, maxOutputPerLine = 50);
    wsOutput.generateWorksheetOutput(); // block until WS done.

    wsOutput.outputPerLine.get(5) match {
      case Some(xs) => assertResult(50)(xs.length);
      case None => fail("No output was given.");
    }
    wsOutput.outputPerLine.get(8) match {
      case Some(xs) => assertResult(50 + 1)(xs.length);
      case None => fail("No output was given.");
    }
  }

  it should "timeout for infinite loops." in {
    // It's 'arbitrary' whether the 'cropping' here should be done from
    //  Worksheetify, or the 'UI' (Wsfy->String).
    // In either case, 50 lines of output for one line is excessive.

    val inputProgram = """#include <stdio.h>
int main(int argc, char **argv) { // Line 02
  int x = 1;
  while (1) {
    x = 1 - x;
  }
}""";

    // Timeout if not complete after some time.
    // (When MAX_ITERATIONS is 10k, it takes ~3-4 seconds on my computer).
    val timer = new Timer();
    timer.schedule(new TimerTask() {
      override def run() : Unit = {
        fail("Shouldn't have timed out.")
      }
    }, 4000);

    val wsOutput = Worksheetify.worksheetifyForInput(inputProgram);
    val wsOutputStr = wsOutput.generateWorksheetOutput(); // block until done.

    // Cancel the timeout.
    timer.cancel();

    // Check for a "timed out" message?
    assert(true);
  }

  it should "timeout for infinite loops (in braceless iteration statements)" in {
    // It's 'arbitrary' whether the 'cropping' here should be done from
    //  Worksheetify, or the 'UI' (Wsfy->String).
    // In either case, 50 lines of output for one line is excessive.

    val inputProgram = """#include <stdio.h>
int main(int argc, char **argv) { // Line 02
  int x = 1;
  while (1)
    x = 1 - x;
}""";

    // Timeout if not complete after some time.
    // (When MAX_ITERATIONS is 10k, it takes ~3-4 seconds on my computer).
    val timer = new Timer();
    timer.schedule(new TimerTask() {
      override def run() : Unit = {
        fail("Shouldn't have timed out.");
      }
    }, 4000);

    val wsOutput = Worksheetify.worksheetifyForInput(inputProgram);
    val wsOutputStr = wsOutput.generateWorksheetOutput(); // block until done.

    // Cancel the timeout.
    timer.cancel();

    // Check for a "timed out" message?
    assert(true);
  }

  it should "be able to adjust max iterations value" in {
    val inputProgram = """#include <stdio.h>
int main(int argc, char **argv) {
  for (int i = 0; i < 5; i++) { // Line 03
    printf("%d\n", i);
  }
}""";

    val maxIter = 2;
    val wsOutput = Worksheetify.worksheetifyForInput(inputProgram, maxIterations = maxIter)
    val wsOutputStr = wsOutput.generateWorksheetOutput(); // block until done.

    wsOutput.outputPerLine.get(4) match {
      case Some(xs) => assert(xs.last.contains("max iterations exceeded"));
      case None => fail("No output was given.");
    }
  }

  // These are somewhat special case.
  // Expects e.g. "Abc" //> Abc
  it should "char-arrays should output the same as pointer-to-char (strings)" in {
    val inputProgram = """int main(int argc, char **argv) {
  char ac[] = "Hello";
  char *pc = "Hello";
  ac;
  pc;
}""";

    val maxIter = 2;
    val wsOutput = Worksheetify.worksheetifyForInput(inputProgram, maxIterations = maxIter)
    val wsOutputStr = wsOutput.generateWorksheetOutput(); // block until done.

    val line4 = wsOutput.outputPerLine.get(4);
    val line5 = wsOutput.outputPerLine.get(5);

    assert(line4.equals(line5));
  }

  it should "char-arrays should output the same as string-literal (strings)" in {
    val inputProgram = """int main(int argc, char **argv) {
  char ac[] = "Hello";
  ac;
  "Hello";
}""";

    val maxIter = 2;
    val wsOutput = Worksheetify.worksheetifyForInput(inputProgram, maxIterations = maxIter)
    val wsOutputStr = wsOutput.generateWorksheetOutput(); // block until done.

    val line3 = wsOutput.outputPerLine.get(3);
    val line4 = wsOutput.outputPerLine.get(4);

    assert(line3.equals(line4));
  }
}
