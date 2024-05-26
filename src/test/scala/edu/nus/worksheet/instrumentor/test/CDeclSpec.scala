package edu.nus.worksheet.instrumentor.test

import org.scalatest._
import flatspec._
import edu.nus.worksheet.instrumentor._

class CDeclSpec extends AnyFlatSpec {

  def assertGibberishToEnglish(gibberish: String, expectedEnglish: String): Unit = {
    val actualEnglish = CDecl.gibberishToEnglish(gibberish);
    assertResult(expectedEnglish)(actualEnglish);
  }

  "CDecl" should "explain basic declarations" in {
    assertGibberishToEnglish("int x;", "x is int");

    assertGibberishToEnglish("int *x;", "x is pointer to int");
  }

  it should "explain array declarations" in {
    assertGibberishToEnglish("int x[4];", "x is array 4 of int");

    assertGibberishToEnglish("int x[5][3];", "x is array 5 of array 3 of int");
  }

  it should "explain array declarations with topmost dimension ommitted" in {
    assertGibberishToEnglish("int x[] = { 1, 2, 3 };", "x is array of int");
  }

  it should "explain const pointers" in {
    assertGibberishToEnglish("int * const x;", "x is const pointer to int");

    assertGibberishToEnglish(
      "const int * const x;",
      "x is const pointer to const int"
    );
  }

  it should "explain pointer-to-void" in {
    assertGibberishToEnglish("void * x;", "x is pointer to void");
  }

  it should "explain pointers/arrays (mixed)" in {
    assertGibberishToEnglish("int *x[3];", "x is array 3 of pointer to int");

    assertGibberishToEnglish(
      "int (*x[5])[3];",
      "x is array 5 of pointer to array 3 of int"
    );

    assertGibberishToEnglish(
      "int *(*x)[3];",
      "x is pointer to array 3 of pointer to int"
    );

    assertGibberishToEnglish(
      "int (**x)[3];",
      "x is pointer to pointer to array 3 of int"
    );

    assertGibberishToEnglish(
      "int **x[3];",
      "x is array 3 of pointer to pointer to int"
    );
  }

  it should "explain function pointers" in {
    assertGibberishToEnglish(
      "int (*x)(void);",
      "x is pointer to function (void) returning int"
    );

    assertGibberishToEnglish(
      "int (*x)(int, char*);",
      "x is pointer to function (int, pointer to char) returning int"
    );

    // from cdecl.org
    assertGibberishToEnglish(
      "int (*(*foo)(void ))[3];",
      "foo is pointer to function (void) returning pointer to array 3 of int"
    );
  }

}
