package edu.nus.worksheet.instrumentor.test

import org.antlr.v4.runtime._
import org.antlr.v4.runtime.tree._
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

  it should "describe pointers" in {
    val input = """int *x;""";
    val expected = PointerType("x", PrimitiveType("(*x)", "int"));
    val actual = StringConstruction.getCTypeOf(input);

    assertResult(expected)(actual);
  }

  it should "describe typedefs to pointers" in {
    val input = """typedef int * ptrToInt;
                   ptrToInt x;""";
    val expected = PointerType("x", PrimitiveType("(*x)", "int"));
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
    val expected = PointerType("intPtr", PrimitiveType("(*intPtr)", "int"));
    val actual = StringConstruction.getCTypeOf(input);

    assertResult(expected)(actual);
  }

  it should "describe array-of-pointer declarations" in {
    val input = "int (*arrayOfPtr[5])[3];";
    val expected = ArrayType("arrayOfPtr",
                             "arrayOfPtr_0",
                             "5",
                             PointerType("arrayOfPtr[arrayOfPtr_0]",
                                         ArrayType("(*arrayOfPtr[arrayOfPtr_0])",
                                                   "arrayOfPtr_1",
                                                   "3",
                                                   PrimitiveType("(*arrayOfPtr[arrayOfPtr_0])[arrayOfPtr_1]", "int"))));
    val actual = StringConstruction.getCTypeOf(input);

    assertResult(expected)(actual);
  }

  it should "describe pointer-of-array declarations" in {
    val input = "int *(*ptrToArr)[3];";
    val expected = PointerType("ptrToArr",
                               ArrayType("(*ptrToArr)",
                                         "ptrToArr_0",
                                         "3",
                                         PointerType("(*ptrToArr)[ptrToArr_0]",
                                                     PrimitiveType("(*(*ptrToArr)[ptrToArr_0])",
                                                                   "int"))));
    val actual = StringConstruction.getCTypeOf(input);

    assertResult(expected)(actual);
  }

  it should "describe enums" in {
    val input = "enum MyEnum { FOO, BAR } myEnum;";
    val expected = EnumType("myEnum", "MyEnum", Seq("FOO", "BAR"));
    val actual = StringConstruction.getCTypeOf(input);

    assertResult(expected)(actual);
  }

  it should "describe simple structs" in {
    val input = "struct MyStruct { int x; float y; } myStruct;";
    val expected = StructType("myStruct", "struct", "MyStruct", Seq(PrimitiveType("myStruct.x", "int"),
                                                    PrimitiveType("myStruct.y", "float")));
    val actual = StringConstruction.getCTypeOf(input);

    assertResult(expected)(actual);
  }

  it should "describe simple unions" in {
    val input = "union MyUnion { int x; float y; } myUnion;";
    val expected = StructType("myUnion", "union", "MyUnion", Seq(PrimitiveType("myUnion.x", "int"),
                                                                 PrimitiveType("myUnion.y", "float")));
    val actual = StringConstruction.getCTypeOf(input);

    assertResult(expected)(actual);
  }

  it should "describe anonymous structs" in {
    val input = "struct { int x; float y; } myStruct;";
    val expected = StructType("myStruct", "struct", null, Seq(PrimitiveType("myStruct.x", "int"),
                                                    PrimitiveType("myStruct.y", "float")));
    val actual = StringConstruction.getCTypeOf(input);

    assertResult(expected)(actual);
  }

  it should "describe structs within structs" in {
    val input = "struct MyStruct{ int x; struct {int x;} y; } myStruct;";
    val innerStruct = StructType("myStruct.y", "struct", null, Seq(PrimitiveType("myStruct.y.x", "int")));

    val expected = StructType("myStruct", "struct", "MyStruct", Seq(PrimitiveType("myStruct.x", "int"),
                                                                    innerStruct));
    val actual = StringConstruction.getCTypeOf(input);

    assertResult(expected)(actual);
  }

  it should "describe structs within structs (deeper)" in {
    val input = "struct MyStruct{ int x; struct {int x; struct {int x;} z;} y; } myStruct;";
    val innermostStruct = StructType("myStruct.y.z", "struct", null, Seq(PrimitiveType("myStruct.y.z.x", "int")));
    val innerStruct = StructType("myStruct.y", "struct", null, Seq(PrimitiveType("myStruct.y.x", "int"),
                                                                   innermostStruct));
    val expected = StructType("myStruct", "struct", "MyStruct", Seq(PrimitiveType("myStruct.x", "int"),
                                                                    innerStruct));
    val actual = StringConstruction.getCTypeOf(input);

    assertResult(expected)(actual);
  }

  it should "describe structs from previously declared struct type" in {
    val input = """struct MyStruct { int x; float y; };
                   struct MyStruct myStruct;""";
    val expected = StructType("myStruct", "struct", "MyStruct", Seq(PrimitiveType("myStruct.x", "int"),
                                                                    PrimitiveType("myStruct.y", "float")));
    val actual = StringConstruction.getCTypeOf(input);

    assertResult(expected)(actual);
  }

  it should "describe structs from a typedef'd struct" in {
    val input = """struct MyStruct { int x; float y; };
                   typedef struct MyStruct MyStruct_t;
                   MyStruct_t myStruct;""";
    val expected = StructType("myStruct", "struct", "MyStruct", Seq(PrimitiveType("myStruct.x", "int"),
                                                                    PrimitiveType("myStruct.y", "float")));
    val actual = StringConstruction.getCTypeOf(input);

    assertResult(expected)(actual);
  }

  it should "describe structs from a typedef'd struct with the same tag" in {
    val input = """struct MyStruct { int x; float y; };
                   typedef struct MyStruct MyStruct;
                   MyStruct myStruct;""";
    val expected = StructType("myStruct", "struct", "MyStruct", Seq(PrimitiveType("myStruct.x", "int"),
                                                                    PrimitiveType("myStruct.y", "float")));
    val actual = StringConstruction.getCTypeOf(input);

    assertResult(expected)(actual);
  }

  it should "describe simple function definitions" in {
    val input = """float f(int x, char y) { return x; }""";
    val expected = FunctionType("f",
                                PrimitiveType(null, "float"),
                                Seq(PrimitiveType("x", "int"), PrimitiveType("y", "char")));
    val actual = StringConstruction.getCTypeOf(input);

    assertResult(expected)(actual);
  }

  it should "describe simple function prototypes" in {
    val input = """float f(int x, char y);""";
    val expected = FunctionType("f",
                                PrimitiveType(null, "float"),
                                Seq(PrimitiveType("x", "int"), PrimitiveType("y", "char")));
    val actual = StringConstruction.getCTypeOf(input);

    assertResult(expected)(actual);
  }

  it should "work for K&R style function declarations" in {
    // While this is "bad style",
    // and will produce "incorrect" C programs ... probably
    // not uncommon to see K&R declarations mixed with ISO C definitions.
    val input = """int f();
                   int f(float x, char y) { return 0; }""";
    val expected = FunctionType("f",
                                PrimitiveType(null, "int"),
                                Seq(PrimitiveType("x", "float"), PrimitiveType("y", "char")));
    val actual = StringConstruction.getCTypeOf(input);

    assertResult(expected)(actual);
  }

  ignore should "work for K&R style function declaration & definition" in {
    // At the moment, my ANTLR grammar cannot parse `f(x,y)`.
    val input = """int f();
                   int f (x, y) float x; char y; { return 0; }""";
    val expected = FunctionType("f",
                                PrimitiveType(null, "int"),
                                Seq(PrimitiveType("x", "float"), PrimitiveType("y", "char")));
    val actual = StringConstruction.getCTypeOf(input);

    assertResult(expected)(actual);
  }

  // It's now assumed that whatever works for function prototypes
  // will work equivalently for function definitions.

  it should "describe simple function prototypes with abstract declarators" in {
    val input = """float f(int, char);""";
    val expected = FunctionType("f",
                                PrimitiveType(null, "float"),
                                Seq(PrimitiveType(null, "int"), PrimitiveType(null, "char")));
    val actual = StringConstruction.getCTypeOf(input);

    assertResult(expected)(actual);
  }

  it should "describe function prototypes, parameter has pointer" in {
    val input = """float f(int *x);""";
    val expected = FunctionType("f",
                                PrimitiveType(null, "float"),
                                Seq(PointerType("x", PrimitiveType("(*x)", "int"))));
    val actual = StringConstruction.getCTypeOf(input);

    assertResult(expected)(actual);
  }

  it should "describe function prototypes, parameter has pointer, with abstract declarators" in {
    val input = """float f(int *);""";
    val expected = FunctionType("f",
                                PrimitiveType(null, "float"),
                                Seq(PointerType(null, PrimitiveType(null, "int"))));
    val actual = StringConstruction.getCTypeOf(input);

    assertResult(expected)(actual);
  }

  it should "describe function prototypes, parameter has array" in {
    val input = """float f(int x[3]);""";
    val expected = FunctionType("f",
                                PrimitiveType(null, "float"),
                                Seq(ArrayType("x", "x_0", "3", PrimitiveType("x[x_0]", "int"))));
    val actual = StringConstruction.getCTypeOf(input);

    assertResult(expected)(actual);
  }

  it should "describe function prototypes, parameter has array, with abstract declarators" in {
    val input = """float f(int[3]);""";
    val expected = FunctionType("f",
                                PrimitiveType(null, "float"),
                                Seq(ArrayType(null, null, "3", PrimitiveType(null, "int"))));
    val actual = StringConstruction.getCTypeOf(input);

    assertResult(expected)(actual);
  }

  it should "describe function pointers" in {
    val input = """int (*fp)(int);""";
    val expected = PointerType("fp",
                               FunctionType("(*fp)",
                                            PrimitiveType(null, "int"),
                                            Seq(PrimitiveType(null, "int"))));
    val actual = StringConstruction.getCTypeOf(input);

    assertResult(expected)(actual);
  }

  it should "describe typedefs to function pointers" in {
    val input = """typedef int (*fpIntRtnInt)(int);
                   fpIntRtnInt fp;""";
    val expected = PointerType("fp",
                               FunctionType("(*fp)",
                                            PrimitiveType(null, "int"),
                                            Seq(PrimitiveType(null, "int"))));
    val actual = StringConstruction.getCTypeOf(input);

    assertResult(expected)(actual);
  }

  it should "describe function prototypes, parameter has function pointer" in {
    val input = """float f(int (*fp)(int));""";
    val fpType = FunctionType("(*fp)",
                              PrimitiveType(null, "int"),
                              Seq(PrimitiveType(null, "int")));
    val expected = FunctionType("f",
                                PrimitiveType(null, "float"),
                                Seq(PointerType("fp", fpType)));
    val actual = StringConstruction.getCTypeOf(input);

    assertResult(expected)(actual);
  }

  it should "describe function prototypes, parameter has function pointer, with abstract declarator" in {
    val input = """float f(int (*)(int));""";
    val fpType = FunctionType(null,
                              PrimitiveType(null, "int"),
                              Seq(PrimitiveType(null, "int")));
    val expected = FunctionType("f",
                                PrimitiveType(null, "float"),
                                Seq(PointerType(null, fpType)));
    val actual = StringConstruction.getCTypeOf(input);

    assertResult(expected)(actual);
  }

  it should "describe function prototypes, returning pointer to primitive" in {
    val input = """float *f(int x);""";
    val expected = FunctionType("f",
                                PointerType(null, PrimitiveType(null, "float")),
                                Seq(PrimitiveType("x", "int")));
    val actual = StringConstruction.getCTypeOf(input);

    assertResult(expected)(actual);
  }

  it should "describe function prototypes, returning function pointer" in {
    val input = """float (*f(int x))(char);""";
    val fpType = FunctionType(null,
                              PrimitiveType(null, "float"),
                              Seq(PrimitiveType(null, "char")));
    val expected = FunctionType("f",
                                PointerType(null, fpType),
                                Seq(PrimitiveType("x", "int")));
    val actual = StringConstruction.getCTypeOf(input);

    assertResult(expected)(actual);
  }

  it should "describe function prototypes, returning function pointer, with abstract declarator" in {
    val input = """float (*f(int))(char);""";
    val fpType = FunctionType(null,
                              PrimitiveType(null, "float"),
                              Seq(PrimitiveType(null, "char")));
    val expected = FunctionType("f",
                                PointerType(null, fpType),
                                Seq(PrimitiveType(null, "int")));
    val actual = StringConstruction.getCTypeOf(input);

    assertResult(expected)(actual);
  }

  def ctypeOfTypeName(typeName : String) : CType = {
    // typeName is not a declaration/function, so this test needs to be a bit custom.
    val input = new ANTLRInputStream(typeName);
    val lexer = new CLexer(input);
    val tokens = new CommonTokenStream(lexer);
    val parser = new CParser(tokens);

    val walker = new ParseTreeWalker();
    val tree = parser.typeName(); // entry point for this unit test

    val defineScopesPhase = new DefineScopesPhase();
    walker.walk(defineScopesPhase, tree);
    val scopes = defineScopesPhase.scopes;

    val ctypeFromDecl = new CTypeFromDeclaration(scopes);
    val strCons = new StringConstruction(scopes);
    walker.walk(strCons, tree);

    // Need to clean up any forward declarations.
    defineScopesPhase.allScopes.foreach(_.flattenForwardDeclarations());

    return ctypeFromDecl.ctypeOf(tree);
  }

  it should "describe typeNames correctly for int (*[])" in {
    val typeName = "int (*[])";
    val typeNameCt = ctypeOfTypeName(typeName);

    val expectedCt = ArrayType(null, null, null, PointerType(null, PrimitiveType(null, "int")));
    assertResult(expectedCt)(typeNameCt);
  }

  it should "describe typeNames correctly for int (*)[]" in {
    // typeName is not a declaration/function, so this test needs to be a bit custom.
    val typeName = "int (*)[]";
    val typeNameCt = ctypeOfTypeName(typeName);

    val expectedCt = PointerType(null, ArrayType(null, null, null, PrimitiveType(null, "int")));
    assertResult(expectedCt)(typeNameCt);
  }

  it should "describe typeNames correctly for int *(*[])()" in {
    // typeName is not a declaration/function, so this test needs to be a bit custom.
    val typeName = "int *(*[])()";
    val typeNameCt = ctypeOfTypeName(typeName);

    val expectedCt = ArrayType(null,
                               null,
                               null,
                               PointerType(null,
                                           FunctionType(null,
                                                        PointerType(null,
                                                                    PrimitiveType(null, "int")),
                                                        Seq())));
    assertResult(expectedCt)(typeNameCt);
  }

  it should "be able to handle forward declarations" in {
    val input = """struct S;
                   typedef struct S S_t;
                   struct S { int x; };
                   S_t x;""";
    val expected = StructType("x",
                              "struct",
                              "S",
                              Seq(PrimitiveType("x.x", "int")));
    val actual = StringConstruction.getCTypeOf(input);

    assertResult(expected)(actual);
  }

  it should "be able to handle functions with varargs" in {
    val input = """int f(int n, ...) { return 5; }""";
    val actual = StringConstruction.getCTypeOf(input);
    val expected = FunctionType("f",
                                PrimitiveType(null, "int"),
                                Seq(PrimitiveType("n", "int"),
                                    VarArgType()));

    assertResult(expected)(actual);
  }
}