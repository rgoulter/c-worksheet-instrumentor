package edu.nus.worksheet.instrumentor.test

import org.scalatest._
import edu.nus.worksheet._
import edu.nus.worksheet.instrumentor._
import edu.nus.worksheet.instrumentor.TypeInference.inferType;

class TypeInferenceSpec extends FlatSpec {

  "Type inference" should "work for simple constants" in {
    assertResult(PrimitiveType(null, "int"))(inferType(null, "5"));
    assertResult(PrimitiveType(null, "double"))(inferType(null, "5.34"));
    assertResult(PrimitiveType(null, "char"))(inferType(null, "'x'"));
  }
}