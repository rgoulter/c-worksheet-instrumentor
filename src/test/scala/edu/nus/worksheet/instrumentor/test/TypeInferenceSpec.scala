package edu.nus.worksheet.instrumentor.test

import org.scalatest._
import edu.nus.worksheet._
import edu.nus.worksheet.instrumentor._
import edu.nus.worksheet.instrumentor.TypeInference.inferType;

class TypeInferenceSpec extends FlatSpec {
  def assertInference(result : Any, program : String, expr : String) {
    val inferredType = inferType(program, expr);
    assertResult(result)(inferredType);
  }

  "Type inference" should "work for simple constants" in {
    assertInference(PrimitiveType("5", "int"), null, "5");
    assertInference(PrimitiveType("5.34", "double"), null, "5.34");
    assertInference(PrimitiveType("'x'", "char"), null, "'x'");
  }

  it should "work for primary expressions" in {
    assertInference(PrimitiveType("\"Abc\"", "string"), null, "\"Abc\"");
    assertInference(PrimitiveType("\"Abcdef\"", "string"), null, "\"Abc\" \"def\"");

    assertInference(PrimitiveType("(5)", "int"), null, "(5)");

    // If infering type of a variable, return the same id.
    assertInference(PrimitiveType("x", "int"), "int x;", "x");

    // Inferring for 'complex' structures,
    // TypeInference should "build up" to the expression. e.g.
    // for expression x[2], the derived "type" should be Primitive("x[2]", "int"),
    // not "x[x_0]". Thus, need this:
    assertInference(ArrayType("x", "x_0", "4", PrimitiveType("x[x_0]", "int")), "int x[4];", "x");
  }

  it should "infer postfix expressions" in {
    assertInference(PrimitiveType("x[0]", "int"), "int x[2] = {1,2};", "x[0]");
    assertInference(PrimitiveType("s.x", "int"), "struct {int x;} s;", "s.x");
    assertInference(PrimitiveType("(*p).x", "int"), "struct S {int x;} s; struct S *p = &s;", "p->x");
    assertInference(PrimitiveType("i++", "int"), "int i;", "i++");
  }

  it should "infer postfix function calls" in {
    assertInference(PrimitiveType("f()", "int"), "int f(int x) { return 3; }", "f()");
    assertInference(PrimitiveType("(*g)(3)", "int"), "int (*g)(int);", "(*g)(3)");
    assertInference(PrimitiveType("g(3)", "int"), "int (*g)(int);", "g(3)");
    assertInference(PrimitiveType("g()", "int"), "int (*g)();", "g()");
  }

  it should "infer postfix compound literals" in {
    assertInference(StructType("(union unIntFloat) { i }",
                               "union",
                               "unIntFloat",
                               Seq(PrimitiveType("(union unIntFloat) { i }.i", "int"),
                                   PrimitiveType("(union unIntFloat) { i }.f", "float"))),
                    "union unIntFloat {int i; float f; }; int i = 3;",
                    "(union unIntFloat) { i }");

    // TODO: test anonymous structs/unions
    // TODO: tests array of func ptrs (where fun returns ptr).
    //       (as a way of checking typename -> string).
  }

  it should "infer infix expressions" in {
    assertInference(PrimitiveType("++i", "int"), "int i;", "++i");
    assertInference(PrimitiveType("!i", "int"), "int i;", "!i");
    assertInference(PointerType("&i", PrimitiveType("i", "int")), "int i;", "&i");
    assertInference(PrimitiveType("(*p)", "int"), "int *p;", "*p");
    assertInference(PrimitiveType("sizeof p", "size_t"), "int p;", "sizeof p");
  }

  it should "infer cast expressions" in {
    assertInference(PrimitiveType("(long) i", "long"), "int i;", "(long) i");
  }

  it should "infer 'arithmetic' expressions (multiplication, addition, shifting)" in {
    assertInference(PrimitiveType("2 * 3", "int"), null, "2 * 3");
    assertInference(PrimitiveType("2 + 3", "int"), null, "2 + 3");
    assertInference(PrimitiveType("8 >> 2", "int"), null, "8 >> 2");
  }

  ignore should "infer pointer-arithmetic expressions" in {
    // This case is trickier, since it adds parentheses when
    // dereferencing pointer.
    assertInference(PointerType("p + 3", PrimitiveType("*(p + 3)", "int")), "int *p;", "p + 3");
  }

  it should "infer 'comparison' expressions (lt, eq, lt-eq)" in {
    assertInference(PrimitiveType("2 > 3", "int"), null, "2 > 3");
    assertInference(PrimitiveType("2 >= 3", "int"), null, "2 >= 3");
    assertInference(PrimitiveType("2 == 3", "int"), null, "2 == 3");
  }

  it should "infer 'bitwise' expressions (and, or, xor)" in {
    assertInference(PrimitiveType("2 & 3", "int"), null, "2 & 3");
    assertInference(PrimitiveType("2 ^ 3", "int"), null, "2 ^ 3");
    assertInference(PrimitiveType("2 | 3", "int"), null, "2 | 3");
  }

  it should "infer 'logical' expressions (and, or)" in {
    assertInference(PrimitiveType("0 && 1", "int"), null, "0 && 1");
    assertInference(PrimitiveType("0 || 1", "int"), null, "0 || 1");
  }

  it should "infer 'compound' expressions (ternary, assignment, comma-op)" in {
    assertInference(PrimitiveType("0 ? 3 : 4", "int"), null, "0 ? 3 : 4");
    assertInference(PrimitiveType("x = 3", "int"), "int x;", "x = 3");
    assertInference(PrimitiveType("3, 4", "int"), null, "3, 4");

    // TODO: ternary op can be quite complicated. May be worth adding more asserts.
    // TODO: I could do with more tests for *chained* assignment expressions
  }
}