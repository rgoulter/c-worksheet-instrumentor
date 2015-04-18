package edu.nus.worksheet.instrumentor.test

import org.scalatest._
import edu.nus.worksheet.instrumentor._
import edu.nus.worksheet.instrumentor.CTypeToDeclaration.declarationOf;
import edu.nus.worksheet.instrumentor.CTypeToDeclaration.stringOfTypeName;

class CTypeToDeclarationSpec extends FlatSpec {
	"CType Declaration" should "work for primitives" in {
    val expected = "int x";
    val t = PrimitiveType(null, "int");
    val actual = declarationOf(t, "x");

    assertResult(expected)(actual);
  }

	it should "work for arrays" in {
    val expected = "int x[3][4]";
    val t = ArrayType(null,
                      "i",
                      "3",
                      ArrayType(null,
                                "j",
                                "4",
                                PrimitiveType(null, "int")));
    val actual = declarationOf(t, "x");

    assertResult(expected)(actual);
  }

	it should "work for simple pointers" in {
    val expected = "int *x";
    val t = PointerType(null,
                        PrimitiveType(null, "int"));
    val actual = declarationOf(t, "x");

    assertResult(expected)(actual);
  }

	it should "work for pointers of arrays" in {
    val expected = "int (*x)[3]";
    val t = PointerType(null,
                        ArrayType(null,
                                  "i",
                                  "3",
                                  PrimitiveType(null, "int")));
    val actual = declarationOf(t, "x");

    assertResult(expected)(actual);
  }

	it should "work for array of pointers" in {
    val expected = "int *x[3]";
    val t = ArrayType(null,
                      "i",
                      "3",
                      PointerType(null,
                                  PrimitiveType(null, "int")));
    val actual = declarationOf(t, "x");

    assertResult(expected)(actual);
  }

	it should "work for pointer to array of pointers" in {
    val expected = "int *(*x)[3]";
    val t = PointerType(null,
                        ArrayType(null,
                                  "i",
                                  "3",
                                  PointerType(null,
                                              PrimitiveType(null, "int"))));
    val actual = declarationOf(t, "x");

    assertResult(expected)(actual);
  }

  it should "work for simple function pointers" in {
    val expected = "int (*x)(int)";
    val t = PointerType(null,
                        FunctionType(null,
                                     PrimitiveType(null, "int"),
                                     Seq(PrimitiveType(null, "int"))));
    val actual = declarationOf(t, "x");

    assertResult(expected)(actual);
  }

  it should "work for array of pointers to functions which return a pointer" in {
    // inverse of a test in StringConstructionSpec
    val expected = "int * (*x[])()";

    val t = ArrayType(null,
                      null,
                      null,
                      PointerType(null,
                                  FunctionType(null,
                                               PointerType(null,
                                                           PrimitiveType(null, "int")),
                                               Seq())));
    val actual = declarationOf(t, "x");
    assertResult(expected)(actual);
  }

  it should "recover correct string for typename `int (*[])`" in {
    // Need to be able to get the typeName correct!
    val typeName = "int *[]";
    val typeNameCt = ArrayType(null, null, null, PointerType(null, PrimitiveType(null, "int")));

    val result = stringOfTypeName(typeNameCt);

    assertResult(typeName)(result);
  }

  it should "recover correct string for typename `int (*)[]`" in {
    // Need to be able to get the typeName correct!
    val typeName = "int (*)[]";
    val typeNameCt = PointerType(null, ArrayType(null, null, null, PrimitiveType(null, "int")));

    val result = stringOfTypeName(typeNameCt);

    assertResult(typeName)(result);
  }
}