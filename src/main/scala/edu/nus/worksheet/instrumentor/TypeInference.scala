package edu.nus.worksheet.instrumentor

import org.antlr.v4.runtime._
import org.antlr.v4.runtime.tree._
import scala.collection.JavaConversions._

// Return a CType from some expression.
//
// CType returned doesn't necessarily have an id, but it might.
class TypeInference(scope : Scope[CType]) extends CBaseVisitor[CType] {
  override def visitConstant(ctx : CParser.ConstantContext) : CType = {
    val t : String = if(ctx.IntegerConstant() != null) {
        "int";
      } else if (ctx.FloatingConstant() != null) {
        "double";
      } else if (ctx.CharacterConstant() != null) {
        "char";
      } else {
        throw new Exception("constant must be Integer,Floating or Character Constant");
      }
    return PrimitiveType(null, t);
  }

  override def visitPrimaryExpression(ctx : CParser.PrimaryExpressionContext) : CType = {
    if (ctx.Identifier() != null) {
      val id = ctx.Identifier().getText();
      return scope.resolve(id) match {
        case Some(ct) => ct;
        case None => null; // Type-of an undefined variable? Compiler Error!
      }
    } else if (ctx.constant() != null) {
      return visitConstant(ctx.constant());
    } else if (ctx.StringLiteral().length > 0) {
      return PrimitiveType(null, "string");
    } else if (ctx.expression() != null) {
      return visitExpression(ctx.expression());
    } else {
      throw new UnsupportedOperationException("Unknown/unsupported primaryExpression");
    }
  }
}

object TypeInference {
  // Helper method for creating scopes which we can test with.
  def dummyGlobalScopeFor(program : String) : Scope[CType] = {
    val input = new ANTLRInputStream(program);
    val lexer = new CLexer(input);
    val tokens = new CommonTokenStream(lexer);
    val parser = new CParser(tokens);

    val tree = parser.compilationUnit();

    val walker = new ParseTreeWalker();

    val defineScopesPhase = new DefineScopesPhase[CType]();
    walker.walk(defineScopesPhase, tree);
    val scopes = defineScopesPhase.scopes;

    val strCons = new StringConstruction(tokens, scopes);
    walker.walk(strCons, tree);

    return defineScopesPhase.globals;
  }

  def inferType(scope : Scope[CType], of : String) : CType = {
    val input = new ANTLRInputStream(of);
    val lexer = new CLexer(input);
    val tokens = new CommonTokenStream(lexer);
    val parser = new CParser(tokens);

    val tree = parser.assignmentExpression(); // entry rule for parser

    val tooler = new TypeInference(scope);
    return tooler.visit(tree);
  }

  def p_inferType(scope : Scope[CType], of : String) : CType = {
    println("TypeInfer " + of);
    val t = inferType(scope, of);
    println(t);
    println();
    return t;
  }

  def main(args : Array[String]) : Unit = {
    p_inferType(null, "5");
    p_inferType(null, "5.34");
    p_inferType(null, "'x'");

    p_inferType(null, "\"Abc\"");
    p_inferType(null, "\"Abc\" \"def\"");
    p_inferType(null, "(5)");
    p_inferType(dummyGlobalScopeFor("int x;"), "x");

    p_inferType(dummyGlobalScopeFor("int x[2] = {1,2};"), "x[0]");
    p_inferType(dummyGlobalScopeFor("struct {int x;} s;"), "s.x");
    p_inferType(dummyGlobalScopeFor("struct S {int x;} s; struct S *p = &s;"), "p->x");
    p_inferType(dummyGlobalScopeFor("int i;"), "i++");
    println("Done");
  }
}