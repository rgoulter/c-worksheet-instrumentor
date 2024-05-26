package edu.nus.worksheet.instrumentor.test

import org.scalatest.*
import flatspec.*
import edu.nus.worksheet.instrumentor.*
import edu.nus.worksheet.instrumentor.CTypeToDeclaration.declarationOf;
import edu.nus.worksheet.instrumentor.CTypeToDeclaration.stringOfTypeName;

class CTypeToDeclarationSpec extends AnyFlatSpec {
  "CType Declaration" should "work for primitives" in {
    val expected = "int x";
    val t = PrimitiveType(None, "int");
    val actual = declarationOf(t, "x");

    assertResult(expected)(actual);
  }

  it should "work for arrays" in {
    val expected = "int x[3][4]";
    val t = new ArrayType(
      null,
      "i",
      "3",
      new ArrayType(null, "j", "4", PrimitiveType(None, "int"))
    );
    val actual = declarationOf(t, "x");

    assertResult(expected)(actual);
  }

  it should "work for simple pointers" in {
    val expected = "int *x";
    val t = PointerType(None, PrimitiveType(None, "int"));
    val actual = declarationOf(t, "x");

    assertResult(expected)(actual);
  }

  it should "work for pointers of arrays" in {
    val expected = "int (*x)[3]";
    val t = PointerType(
      None,
      ArrayType(None, Some("i"), Some("3"), PrimitiveType(None, "int"))
    );
    val actual = declarationOf(t, "x");

    assertResult(expected)(actual);
  }

  it should "work for array of pointers" in {
    val expected = "int *x[3]";
    val t = ArrayType(
      None,
      Some("i"),
      Some("3"),
      PointerType(None, PrimitiveType(None, "int"))
    );
    val actual = declarationOf(t, "x");

    assertResult(expected)(actual);
  }

  it should "work for pointer to array of pointers" in {
    val expected = "int *(*x)[3]";
    val t = PointerType(
      None,
      ArrayType(
        None,
        Some("i"),
        Some("3"),
        PointerType(None, PrimitiveType(None, "int"))
      )
    );
    val actual = declarationOf(t, "x");

    assertResult(expected)(actual);
  }

  it should "work for simple function pointers" in {
    val expected = "int (*x)(int)";
    val t = PointerType(
      None,
      FunctionType(
        None,
        PrimitiveType(None, "int"),
        Seq(PrimitiveType(None, "int"))
      )
    );
    val actual = declarationOf(t, "x");

    assertResult(expected)(actual);
  }

  it should "work for array of pointers to functions which return a pointer" in {
    // inverse of a test in StringConstructionSpec
    val expected = "int * (*x[])()";

    val t = ArrayType(
      None,
      None,
      None,
      PointerType(
        None,
        FunctionType(None, PointerType(None, PrimitiveType(None, "int")), Seq())
      )
    );
    val actual = declarationOf(t, "x");
    assertResult(expected)(actual);
  }

  it should "recover correct string for typename `int (*[])`" in {
    // Need to be able to get the typeName correct!
    val typeName = "int *[]";
    val typeNameCt = ArrayType(
      None,
      None,
      None,
      PointerType(None, PrimitiveType(None, "int"))
    );

    val result = stringOfTypeName(typeNameCt);

    assertResult(typeName)(result);
  }

  it should "recover correct string for typename `int (*)[]`" in {
    // Need to be able to get the typeName correct!
    val typeName = "int (*)[]";
    val typeNameCt = PointerType(
      None,
      ArrayType(None, None, None, PrimitiveType(None, "int"))
    );

    val result = stringOfTypeName(typeNameCt);

    assertResult(typeName)(result);
  }

  it should "work for tagged structs" in {
    // this seems a bit dubious/ambiguous? not outputting the members?
    // for instrumenting, we don't want members.
    val expected = "struct S x";
    val t =
      StructType(None, "struct", Some("S"), Seq(PrimitiveType(None, "int")));
    val actual = declarationOf(t, "x");

    assertResult(expected)(actual);
  }
}
