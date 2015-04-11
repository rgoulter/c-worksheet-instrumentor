package edu.nus.worksheet.instrumentor.test

import org.scalatest._
import edu.nus.worksheet._
import edu.nus.worksheet.instrumentor._
import edu.nus.worksheet.instrumentor.TypeInference.inferType;
import edu.nus.worksheet.instrumentor.TypeInference.dummyGlobalScopeFor;

class TypeInferenceSpec extends FlatSpec {
  def assertInference(result : Any, program : String, expr : String) {
    val scope = if (program != null) dummyGlobalScopeFor(program) else null;
    val inferredType = inferType(scope, expr);
    assertResult(result)(inferredType);
  }

  "Type inference" should "work for simple constants" in {
    assertInference(PrimitiveType(null, "int"), null, "5");
    assertInference(PrimitiveType(null, "double"), null, "5.34");
    assertInference(PrimitiveType(null, "char"), null, "'x'");
  }

  it should "work for primary expressions" in {
    assertInference(PrimitiveType(null, "string"), null, "\"Abc\"");
    assertInference(PrimitiveType(null, "string"), null, "\"Abc\" \"def\"");

    assertInference(PrimitiveType(null, "int"), null, "(5)");

    // If infering type of a variable, return the same id.
    assertInference(PrimitiveType("x", "int"), "int x;", "x");
  }

  it should "infer postfix expressions" in {
    assertInference(PrimitiveType("x[x_0]", "int"), "int x[2] = {1,2};", "x[0]");
    assertInference(PrimitiveType("s.x", "int"), "struct {int x;} s;", "s.x");
    assertInference(PrimitiveType("(*p).x", "int"), "struct S {int x;} s; struct S *p = &s;", "p->x");
    assertInference(PrimitiveType("i", "int"), "int i;", "i++");
  }

  it should "infer infix expressions" in {
    assertInference(PrimitiveType("i", "int"), "int i;", "++i");
    assertInference(PrimitiveType("i", "int"), "int i;", "!i");
    assertInference(PointerType(null, PrimitiveType("i", "int")), "int i;", "&i");
    assertInference(PrimitiveType("(*p)", "int"), "int *p;", "*p");
    assertInference(PrimitiveType(null, "size_t"), "int i;", "sizeof p");
  }
}