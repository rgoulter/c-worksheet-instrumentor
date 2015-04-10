package edu.nus.worksheet.instrumentor.test

import org.scalatest._
import edu.nus.worksheet._
import edu.nus.worksheet.instrumentor._
import edu.nus.worksheet.instrumentor.TypeInference.inferType;
import edu.nus.worksheet.instrumentor.TypeInference.dummyGlobalScopeFor;

class TypeInferenceSpec extends FlatSpec {

  "Type inference" should "work for simple constants" in {
    assertResult(PrimitiveType(null, "int"))(inferType(null, "5"));
    assertResult(PrimitiveType(null, "double"))(inferType(null, "5.34"));
    assertResult(PrimitiveType(null, "char"))(inferType(null, "'x'"));
  }

  it should "work for primary expressions" in {
    assertResult(PrimitiveType(null, "string"))(inferType(null, "\"Abc\""));
    assertResult(PrimitiveType(null, "string"))(inferType(null, "\"Abc\" \"def\""));

    assertResult(PrimitiveType(null, "int"))(inferType(null, "(5)"));

    // If infering type of a variable, return the same id.
    assertResult(PrimitiveType("x", "int"))(inferType(dummyGlobalScopeFor("int x;"), "x"));
  }
}