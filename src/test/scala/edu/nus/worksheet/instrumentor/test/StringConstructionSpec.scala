package edu.nus.worksheet.instrumentor.test

import org.scalatest._
import edu.nus.worksheet.instrumentor._

class StringConstructionSpec extends FlatSpec {

  "String Construction" should "describe a simple declaration" in {
    val input = "int x;";
    val expected = PrimitiveType("x", "int");
    val actual = StringConstruction.getCTypeOf(input);

    assertResult(expected)(actual);
  }

  it should "describe simple declarations" in {
    val input = "int x; float y;";
    val expected = Seq(PrimitiveType("x", "int"), PrimitiveType("y", "float"));
    val actual = StringConstruction.getCTypesOf(input);

    assertResult(expected)(actual);
  }

  it should "describe simple declarations in the same statement" in {
    val input = "int x, y;";
    val expected = Seq(PrimitiveType("x", "int"), PrimitiveType("y", "int"));
    val actual = StringConstruction.getCTypesOf(input);

    assertResult(expected)(actual);
  }
  
  it should "describe declarations with typedef identifiers" in {
    val input = "typedef int myInt; myInt x;";
    val expected = PrimitiveType("x", "int");
    val actual = StringConstruction.getCTypeOf(input);

    assertResult(expected)(actual);
  }
  
  it should "describe do this with multiple typedefs present" in {
    val input = """typedef int myInt;
                   typedef float myFloat;
                   myInt x;""";
    val expected = PrimitiveType("x", "int");
    val actual = StringConstruction.getCTypeOf(input);

    assertResult(expected)(actual);
  }
 
  it should "describe 1D array declarations" in {
    val input = "int x[4];";
    val expected = ArrayType("x", "x_0", "4", PrimitiveType("x[x_0]", "int"));
    val actual = StringConstruction.getCTypeOf(input);

    assertResult(expected)(actual);
  }

  it should "describe multi-dimension array declarations" in {
    val input = "int x[3][4][5];";
    val arrayPrim = PrimitiveType("x[x_0][x_1][x_2]", "int")
    val arrayDim2 = ArrayType("x[x_0][x_1]", "x_2", "5", arrayPrim);
    val arrayDim1 = ArrayType("x[x_0]", "x_1", "4", arrayDim2);
    val expected = ArrayType("x", "x_0", "3", arrayDim1);
    val actual = StringConstruction.getCTypeOf(input);

    assertResult(expected)(actual);
  }

  it should "describe simple pointer declarations" in {
    val input = "int *intPtr;";
    val expected = PointerType("intPtr");
    val actual = StringConstruction.getCTypeOf(input);

    assertResult(expected)(actual);
  }

  it should "describe array-of-pointer declarations" in {
    val input = "int (*arrayOfPtr[5])[3];";
    val expected = ArrayType("arrayOfPtr", "arrayOfPtr_0", "5", PointerType("arrayOfPtr[arrayOfPtr_0]"));
    val actual = StringConstruction.getCTypeOf(input);

    assertResult(expected)(actual);
  }

  it should "describe pointer-of-array declarations" in {
    val input = "int *(*ptrToArr)[3];";
    val expected = PointerType("ptrToArr");
    val actual = StringConstruction.getCTypeOf(input);

    assertResult(expected)(actual);
  }

  it should "describe simple structs" in {
    val input = "struct MyStruct { int x; float y; } myStruct;";
    val expected = StructType("myStruct", "MyStruct", Seq(PrimitiveType("myStruct.x", "int"),
                                                    PrimitiveType("myStruct.y", "float")));
    val actual = StringConstruction.getCTypeOf(input);

    assertResult(expected)(actual);
  }
  
  it should "describe structs from previously declared struct type" in {
    val input = """struct MyStruct { int x; float y; };
                   struct MyStruct myStruct;""";
    val expected = StructType("myStruct", "MyStruct", Seq(PrimitiveType("myStruct.x", "int"),
                                                    PrimitiveType("myStruct.y", "float")));
    val actual = StringConstruction.getCTypeOf(input);

    assertResult(expected)(actual);
    
  }

  it should "describe structs from a typedef'd struct" in {
    val input = """struct MyStruct { int x; float y; };
                   typedef struct MyStruct MyStruct_t;
                   MyStruct_t myStruct;""";
    val expected = StructType("myStruct", "MyStruct", Seq(PrimitiveType("myStruct.x", "int"),
                                                    PrimitiveType("myStruct.y", "float")));
    val actual = StringConstruction.getCTypeOf(input);

    assertResult(expected)(actual);
    
  }

  it should "describe structs from a typedef'd struct with the same tag" in {
    val input = """struct MyStruct { int x; float y; };
                   typedef struct MyStruct MyStruct;
                   MyStruct myStruct;""";
    val expected = StructType("myStruct", "MyStruct", Seq(PrimitiveType("myStruct.x", "int"),
                                                    PrimitiveType("myStruct.y", "float")));
    val actual = StringConstruction.getCTypeOf(input);

    assertResult(expected)(actual);
    
  }
}