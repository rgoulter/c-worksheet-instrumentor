package edu.nus.worksheet.instrumentor.test;

import argonaut.Argonaut;
import Argonaut.{StringToParseWrap, ToJsonIdentity};

import org.scalatest.*;
import flatspec.*;

import edu.nus.worksheet.instrumentor.*;
import CTypeCodec.{CTypeEncodeJson, CTypeDecodeJson};

class CTypeJSONCodecSpec extends AnyFlatSpec {
  def assertIdempotentEncodeAndDecode(ct: CType): Unit = {
    val ctJson = ct.asJson;
    val wire: String = ctJson.nospaces;

    wire.decodeOption[CType] match {
      case Some(decodedCt) =>
        assertResult(ct)(decodedCt);
      case None =>
        fail(s"Couldn't decode for CType $ct");
    }
  }

  "CType JSON Codec" should "be idempotent for primitive types" in {
    val ct = new PrimitiveType("x", "int");
    assertIdempotentEncodeAndDecode(ct);
  }

  // Assume that if null/None id for PrimitiveType is handled correctly,
  // then it's handled correctly for other CTypes.
  it should "be idempotent for primitive types (with null id)" in {
    val ct = new PrimitiveType(null: String, "int");
    assertIdempotentEncodeAndDecode(ct);
  }

  it should "be idempotent for array types" in {
    // n.b. w/ ArrayType; id, idx, n are OptionTypes.

    // id of `of` not important here, (no semantic meaning).
    val of = new PrimitiveType("x", "int");
    val ct = new ArrayType("y", "idx", "n", of);
    assertIdempotentEncodeAndDecode(ct);
  }

  it should "be idempotent for array types (with null idx, n)" in {
    // n.b. w/ ArrayType; id, idx, n are OptionTypes.

    // id of `of` not important here, (no semantic meaning).
    val of = new PrimitiveType("x", "int");
    val ct = new ArrayType(null: String, null, null, of);
    assertIdempotentEncodeAndDecode(ct);
  }

  it should "be idempotent for pointer types" in {
    // id of `of` not important here, (no semantic meaning).
    val of = new PrimitiveType("x", "int");
    val ct = new PointerType("y", of);
    assertIdempotentEncodeAndDecode(ct);
  }

  // CTypeToDeclaration (perhaps mistakenly) has pointer-of-pointer
  // (or pointer-of- anything which can be difficult to visualise)
  // as pointer-to- null.
  it should "be idempotent for pointer-to-null (null of)." in {
    // id of `of` not important here, (no semantic meaning).
    val ct = new PointerType("y", null);
    assertIdempotentEncodeAndDecode(ct);
  }

  it should "be idempotent for struct types" in {
    // id of `of` not important here, (no semantic meaning).
    val of = new PrimitiveType("x", "int");
    val ct = new StructType("y", "struct", "S", Seq(of));
    assertIdempotentEncodeAndDecode(ct);
  }

  it should "be idempotent for struct types (with no members)" in {
    // id of `of` not important here, (no semantic meaning).
    val ct = new StructType("y", "struct", "S", Seq());
    assertIdempotentEncodeAndDecode(ct);
  }

  it should "be idempotent for struct types (with null tag)" in {
    // id of `of` not important here, (no semantic meaning).
    val of = new PrimitiveType("x", "int");
    val ct = new StructType("y", "struct", null, Seq(of));
    assertIdempotentEncodeAndDecode(ct);
  }

  it should "be idempotent for enum types" in {
    val ct = new EnumType("x", "E", Seq("A", "B"));
    assertIdempotentEncodeAndDecode(ct);
  }

  it should "be idempotent for enum types (with null tag)" in {
    val ct = new EnumType("x", null, Seq("A", "B"));
    assertIdempotentEncodeAndDecode(ct);
  }

  it should "be idempotent for function types" in {
    // id of `of` not important here, (no semantic meaning).
    val of = new PrimitiveType("x", "int");
    val ct = new FunctionType("y", of, Seq(of));
    assertIdempotentEncodeAndDecode(ct);
  }

  it should "be idempotent for function types (with no parameters)" in {
    // id of `of` not important here, (no semantic meaning).
    val of = new PrimitiveType("x", "int");
    val ct = new FunctionType("y", of, Seq());
    assertIdempotentEncodeAndDecode(ct);
  }

  // CTypeToDeclaration, TypeInference somewhat ambivalent whether to have `null` as ctype
  //  for returnType of functions which return void.
  // So, for now, have specs/tests for both.

  ignore should "be idempotent for function types (with void return type)" in {
    // id of `of` not important here, (no semantic meaning).
    val of = new PrimitiveType("x", "int");
    val ct = new FunctionType("y", VoidType(), Seq(of));
    assertIdempotentEncodeAndDecode(ct);
  }

  // amazingly, this passes.
  it should "be idempotent for function types (with null return type)" in {
    // id of `of` not important here, (no semantic meaning).
    val of = new PrimitiveType("x", "int");
    val ct = new FunctionType("y", null, Seq(of));
    assertIdempotentEncodeAndDecode(ct);
  }

  it should "be idempotent for function types (with vararg type parameter)" in {
    // id of `of` not important here, (no semantic meaning).
    val of = new PrimitiveType("x", "int");
    val ct = new FunctionType("y", of, Seq(of, VarArgType()));
    assertIdempotentEncodeAndDecode(ct);
  }

  // forward decln.
  it should "be idempotent for ForwardDecl'n types" in {
    // id of `of` not important here, (no semantic meaning).
    val ct = ForwardDeclarationType(Some("fd"), "S");
    assertIdempotentEncodeAndDecode(ct);
  }
}
