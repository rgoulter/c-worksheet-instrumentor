package edu.nus.worksheet.instrumentor.test

import org.scalatest._
import flatspec._
import edu.nus.worksheet.instrumentor._

class CProgramSpec extends AnyFlatSpec {

  val validProgram =
"""#include <stdio.h>
int main(int argc, char** argv) {
  printf("Hello World.");
}"""

  val invalidProgram1 =
"""
int main(int argc, char** argv) {
  printf("Hello World.");
}"""

  val invalidProgram2 =
"""undeclaredIdentifier undecl;"""

  "A valid program" should "not produce warnings" in {
    val prog = new CProgram(validProgram);

    val (warnings, errors) = prog.compile();

    assert(warnings.isEmpty, "No warnings");
    assert(errors.isEmpty, "No warnings");
  }

  // "A program w/ no headers"
  // IGNORE: didn't produce warnings on macos runner.
  ignore should "produce warnings" in {
    val prog = new CProgram(invalidProgram1);

    val (warnings, errors) = prog.compile();

    assert(!warnings.isEmpty, "Warnings");
    assert(errors.isEmpty, "No warnings");
  }

  "A program w/ undeclared identifier" should "produce an error" in {
    val prog = new CProgram(invalidProgram2);

    val (warnings, errors) = prog.compile();

    assert(!errors.isEmpty, "Has an error");
  }

  "Multiple CPrograms" should "be able to compile & run concurrently" in {
    val inputProgram1 = """#include <stdio.h>

int main(int argc, char** argv) { // Line 03
  printf("Foo\n");
}""";
    val inputProgram2 = """#include <stdio.h>

int main(int argc, char** argv) { // Line 03
  printf("Bar\n");
}""";
    val prog1 = new CProgram(inputProgram1);
    val prog2 = new CProgram(inputProgram2);

    // "concurrently", i.e. compile various programs
    // before each is executed.
    prog1.compile();
    prog2.compile();

    val output1 = prog1.process().lineStream.iterator.next();
    val output2 = prog2.process().lineStream.iterator.next();

    assertResult("Foo")(output1);
    assertResult("Bar")(output2);
  }

  "CProgram" should "be able to preprocess some program" in {
    val input = """#define X 5
X"""
    val prog = new CProgram(input);

    prog.preprocessed() match {
      case Some(result) => assert(result.contains("5"))
      case None => fail("This shouldn't produce an error.");
    }
  }

  it should "be able to take in macro definition flags" in {
    val inputProgram = """#include <stdio.h>
#ifndef X
#def X 5
#endif
int main(int argc, char** argv) {
  printf("%d\n", X);
}""";

    val prog = new CProgram(inputProgram, macroDefinitions = Map("X" -> "99"));
    prog.compile();
    val output = prog.process().lineStream.iterator.next();

    assertResult("99")(output);
  }

  it should "be able to take in macro definition flags (for preprocessed program)" in {
    val inputProgram = "X";

    val to = "999123";
    val prog = new CProgram(inputProgram, macroDefinitions = Map("X" -> to));
    prog.preprocessed() match {
      // Can't assert direct equality,
      // As CPP throws some other stuff in.
      case Some(result) => assert(result.contains(to))
      case None => fail("Should be able to preprocess this.");
    }
  }
}
