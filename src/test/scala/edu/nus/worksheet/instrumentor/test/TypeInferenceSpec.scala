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
    assertInference(PrimitiveType("g()", "int"), "int (*g)();", "g()");

    // Void functions ... Should return null or throw exception.
    val voidOfFunProto = inferType("void f();", "f()");
    val voidOfFunDefn = inferType("void f() {}", "f()");
    assert(voidOfFunProto == voidOfFunDefn, "void func definition & prototype should return the same.");

    assertResult(null, "inferred type from calling void (proto). Got " + voidOfFunProto)(voidOfFunProto);
    assertResult(null, "inferred type from calling void (defn). Got " + voidOfFunDefn)(voidOfFunDefn);
  }

  it should "infer postfix compound literals" in {
    assertInference(StructType("(union unIntFloat) { i }",
                               "union",
                               "unIntFloat",
                               Seq(PrimitiveType("(union unIntFloat) { i }.i", "int"),
                                   PrimitiveType("(union unIntFloat) { i }.f", "float"))),
                    "union unIntFloat {int i; float f; }; int i = 3;",
                    "(union unIntFloat) { i }");

    // TODO: tests array of func ptrs (where fun returns ptr).
    //       (as a way of checking typename -> string).
  }

  it should "infer postfix compound literals (arrays)" in {
    // This example is "tricky". TypeName is an array,
    //  but it can't be assigned (or even initialized to) type `int[]`.
    // Must be, in this case, `int*`.
    // So, must convert from Arr(id, _, _, of) to Ptr(id, of).
    val cmpdLitArr = """(int [256]) { [' '] = 1, ['\t'] = 1, ['\n'] = 1, ['\r'] = 1 }""";

    val derefArr = PrimitiveType(s"(*$cmpdLitArr)", "int");
    val expected = PointerType(cmpdLitArr, derefArr);

    // (int[256]){ [' '] = 1, ['\t'] = 1, ['\n'] = 1, ['\r'] = 1 }
    // should be ptr
    assertInference(expected,
                    null,
                    cmpdLitArr);

    // *(int[256]){ [' '] = 1, ['\t'] = 1, ['\n'] = 1, ['\r'] = 1 }
    // should be int. Amazingly.
    assertInference(derefArr,
                    null,
                    "*" + cmpdLitArr);
  }

  it should "infer postfix compound literals (arrays of pointers)" in {
    val cmpdLitPtrs = """(int *[]) { p, q }""";

    val expectedPtr = PointerType(cmpdLitPtrs,
                                  PointerType(s"(*$cmpdLitPtrs)",
                                              PrimitiveType(s"(*(*$cmpdLitPtrs))",
                                                            "int")));

    assertInference(expectedPtr,
                    "int a = 3; int *p = &a; int *q = &a;",
                    cmpdLitPtrs);
  }

  it should "infer postfix compound literals (arrays of function pointers)" in {
    // Separate from above case, because this one really sucks.
    // int * (*[])() is the typename for
    // array-of pointer-to funcptr of func (no args) return pointer to int.
    val cmpdLitFPs = """(int (*[])()) { &f1 }"""

    val expectedFP = PointerType(cmpdLitFPs,  // (coerced) array of
                                 PointerType(s"(*$cmpdLitFPs)", // pointer to
                                             FunctionType(s"(*(*$cmpdLitFPs))",
                                                          PrimitiveType(null, "int"),
                                                          Seq())));
    assertInference(expectedFP,
                    """typedef int (*fPtrInt)();
int f1() {
    static int x = 3;
    return x;
}""",
                    cmpdLitFPs);
  }

  it should "infer postfix compound literals (arrays of function pointers returning pointer)" in {
    // Separate from above case, because this one really sucks.
    // int * (*[])() is the typename for
    // array-of pointer-to funcptr of func (no args) return pointer to int.
    val cmpdLitFPs = """(int * (*[])()) { &f1 }"""

    val expectedFP = PointerType(cmpdLitFPs,
                                 PointerType(s"(*$cmpdLitFPs)",
                                             FunctionType(s"(*(*$cmpdLitFPs))",
                                                          PointerType(null, PrimitiveType(null, "int")),
                                                          Seq())));

    assertInference(expectedFP,
                    """typedef int* (*fPtrInt)();
int* f1() {
    static int x = 3;
    return &x;
}""",
                    cmpdLitFPs);
  }

  it should "infer postfix compound literals (anonymous structs)" in {
    assertInference(StructType("(struct { int x; float y; }) { 3, 4.5f }",
                               "struct",
                               null,
                               Seq(PrimitiveType("(struct { int x; float y; }) { 3, 4.5f }.x", "int"),
                                   PrimitiveType("(struct { int x; float y; }) { 3, 4.5f }.y", "float"))),
                    null,
                    "(struct { int x; float y; }) { 3, 4.5f }");

    assertInference(PrimitiveType("(struct { int x; float y; }) { 3, 4.5f }.x", "int"),
                    null,
                    "(struct { int x; float y; }) { 3, 4.5f }.x");
  }

  it should "infer infix expressions" in {
    assertInference(PrimitiveType("++i", "int"), "int i;", "++i");
    assertInference(PrimitiveType("!i", "int"), "int i;", "!i");
    assertInference(PointerType("&i", PrimitiveType("i", "int")), "int i;", "&i");
    assertInference(PrimitiveType("(*p)", "int"), "int *p;", "*p");
    assertInference(PrimitiveType("sizeof p", "size_t"), "int p;", "sizeof p");
  }

  it should "infer cast expressions" in {
    assertInference(PrimitiveType("(long) i", "long"), "int i;", "(long) i");
  }

  it should "infer 'arithmetic' expressions (multiplication, addition, shifting)" in {
    assertInference(PrimitiveType("2 * 3", "int"), null, "2 * 3");
    assertInference(PrimitiveType("2 + 3", "int"), null, "2 + 3");
    assertInference(PrimitiveType("8 >> 2", "int"), null, "8 >> 2");
  }

  ignore should "infer pointer-arithmetic expressions" in {
    // This case is trickier, since it adds parentheses when
    // dereferencing pointer.
    assertInference(PointerType("p + 3", PrimitiveType("*(p + 3)", "int")), "int *p;", "p + 3");
  }

  it should "infer 'comparison' expressions (lt, eq, lt-eq)" in {
    assertInference(PrimitiveType("2 > 3", "int"), null, "2 > 3");
    assertInference(PrimitiveType("2 >= 3", "int"), null, "2 >= 3");
    assertInference(PrimitiveType("2 == 3", "int"), null, "2 == 3");
  }

  it should "infer 'bitwise' expressions (and, or, xor)" in {
    assertInference(PrimitiveType("2 & 3", "int"), null, "2 & 3");
    assertInference(PrimitiveType("2 ^ 3", "int"), null, "2 ^ 3");
    assertInference(PrimitiveType("2 | 3", "int"), null, "2 | 3");
  }

  it should "infer 'logical' expressions (and, or)" in {
    assertInference(PrimitiveType("0 && 1", "int"), null, "0 && 1");
    assertInference(PrimitiveType("0 || 1", "int"), null, "0 || 1");
  }

  it should "infer 'compound' expressions (ternary, assignment, comma-op)" in {
    assertInference(PrimitiveType("0 ? 3 : 4", "int"), null, "0 ? 3 : 4");
    assertInference(PrimitiveType("x = 3", "int"), "int x;", "x = 3");
    assertInference(PrimitiveType("3, 4", "int"), null, "3, 4");

    // TODO: ternary op can be quite complicated. May be worth adding more asserts.
    // TODO: I could do with more tests for *chained* assignment expressions
  }
}