package edu.nus.worksheet.instrumentor

import org.antlr.v4.runtime._
import org.antlr.v4.runtime.tree._
import scala.collection.JavaConversions._

class TypeInference(scope : Scope) extends CBaseVisitor[CType] {
  override def visitPrimaryExpression(ctx : CParser.PrimaryExpressionContext) : CType = {
    if (ctx.constant() != null) {
      val constant = ctx.constant();
      val t : String = if(constant.IntegerConstant() != null) {
          "int";
        } else if (constant.FloatingConstant() != null) {
          "double";
        } else if (constant.CharacterConstant() != null) {
          "char";
        } else {
          throw new Exception("constant must be Integer,Floating or Character Constant");
        }
      return PrimitiveType(null, t);
    }

    return null;
  }
}

object TypeInference {
  def inferType(scope : Scope, of : String) : CType = {
    println("TypeInfer " + of);
    val input = new ANTLRInputStream(of);
    val lexer = new CLexer(input);
    val tokens = new CommonTokenStream(lexer);
    val parser = new CParser(tokens);

    val tree = parser.assignmentExpression(); // entry rule for parser

    val tooler = new TypeInference(scope);
    return tooler.visit(tree);
  }

  def main(args : Array[String]) : Unit = {
    println(inferType(null, "5"));
    println(inferType(null, "5.34"));
    println(inferType(null, "'x'"));
    println("Done");
  }
}