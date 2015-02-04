package edu.nus.worksheet.instrumentor.test

import org.scalatest._
import edu.nus.worksheet.instrumentor._

class CProgramSpec extends FlatSpec {
  
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

  "A program w/ no headers" should "produce warnings" in {
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
  
}