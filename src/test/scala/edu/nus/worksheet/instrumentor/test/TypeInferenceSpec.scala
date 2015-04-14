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
    assertInference(StructType("(union unIntFloat) { i }", "unIntFloat", Seq(PrimitiveType("i", "int"),PrimitiveType("f", "float"))),
                    "union unIntFloat {int i; float f; }; int i = 3;",
                    "(union unIntFloat) { i }");
  }

  it should "infer infix expressions" in {
    assertInference(PrimitiveType("i", "int"), "int i;", "++i");
    assertInference(PrimitiveType("i", "int"), "int i;", "!i");
    assertInference(PointerType("&i", PrimitiveType("i", "int")), "int i;", "&i");
    assertInference(PrimitiveType("(*p)", "int"), "int *p;", "*p");
    assertInference(PrimitiveType("sizeof p", "size_t"), "int i;", "sizeof p");
  }

  it should "infer cast expressions" in {
    assertInference(PrimitiveType("(long) i", "long"), "int i;", "(long) i");
  }
}