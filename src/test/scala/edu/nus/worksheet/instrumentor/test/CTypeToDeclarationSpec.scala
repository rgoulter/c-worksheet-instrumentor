package edu.nus.worksheet.instrumentor.test

import org.scalatest._
import edu.nus.worksheet.instrumentor._
import edu.nus.worksheet.instrumentor.CTypeToDeclaration.declarationOf;
import edu.nus.worksheet.instrumentor.CTypeToDeclaration.stringOfTypeName;

class CTypeToDeclarationSpec extends FlatSpec {
	"CType Declaration" should "work for primitives" in {
    val expected = "int x";
    val t = PrimitiveType(None, "int");
    val actual = declarationOf(t, "x");

    assertResult(expected)(actual);
  }

	it should "work for arrays" in {
    val expected = "int x[3][4]";
    val t = ArrayType(None,
                      "i",
                      "3",
                      ArrayType(None,
                                "j",
                                "4",
                                PrimitiveType(None, "int")));
    val actual = declarationOf(t, "x");

    assertResult(expected)(actual);
  }

	it should "work for simple pointers" in {
    val expected = "int *x";
    val t = PointerType(None,
                        PrimitiveType(None, "int"));
    val actual = declarationOf(t, "x");

    assertResult(expected)(actual);
  }

	it should "work for pointers of arrays" in {
    val expected = "int (*x)[3]";
    val t = PointerType(None,
                        ArrayType(None,
                                  "i",
                                  "3",
                                  PrimitiveType(None, "int")));
    val actual = declarationOf(t, "x");

    assertResult(expected)(actual);
  }

	it should "work for array of pointers" in {
    val expected = "int *x[3]";
    val t = ArrayType(None,
                      "i",
                      "3",
                      PointerType(None,
                                  PrimitiveType(None, "int")));
    val actual = declarationOf(t, "x");

    assertResult(expected)(actual);
  }

	it should "work for pointer to array of pointers" in {
    val expected = "int *(*x)[3]";
    val t = PointerType(None,
                        ArrayType(None,
                                  "i",
                                  "3",
                                  PointerType(None,
                                              PrimitiveType(None, "int"))));
    val actual = declarationOf(t, "x");

    assertResult(expected)(actual);
  }

  it should "work for simple function pointers" in {
    val expected = "int (*x)(int)";
    val t = PointerType(None,
                        FunctionType(None,
                                     PrimitiveType(None, "int"),
                                     Seq(PrimitiveType(None, "int"))));
    val actual = declarationOf(t, "x");

    assertResult(expected)(actual);
  }

  it should "work for array of pointers to functions which return a pointer" in {
    // inverse of a test in StringConstructionSpec
    val expected = "int * (*x[])()";

    val t = ArrayType(None,
                      null,
                      null,
                      PointerType(None,
                                  FunctionType(None,
                                               PointerType(None,
                                                           PrimitiveType(None, "int")),
                                               Seq())));
    val actual = declarationOf(t, "x");
    assertResult(expected)(actual);
  }

  it should "recover correct string for typename `int (*[])`" in {
    // Need to be able to get the typeName correct!
    val typeName = "int *[]";
    val typeNameCt = ArrayType(None, null, null, PointerType(None, PrimitiveType(None, "int")));

    val result = stringOfTypeName(typeNameCt);

    assertResult(typeName)(result);
  }

  it should "recover correct string for typename `int (*)[]`" in {
    // Need to be able to get the typeName correct!
    val typeName = "int (*)[]";
    val typeNameCt = PointerType(None, ArrayType(None, null, null, PrimitiveType(None, "int")));

    val result = stringOfTypeName(typeNameCt);

    assertResult(typeName)(result);
  }
}