package edu.nus.worksheet.instrumentor

import edu.nus.worksheet.instrumentor.CParser.FunctionDefinitionContext
import edu.nus.worksheet.instrumentor.CParser.DeclaredFunctionDefinitionContext
import edu.nus.worksheet.instrumentor.CParser.DeclaredIdentifierContext
import edu.nus.worksheet.instrumentor.CParser.DeclaredParenthesesContext
import edu.nus.worksheet.instrumentor.CParser.DeclaredArrayContext
import edu.nus.worksheet.instrumentor.CParser.DeclaredFunctionPrototypeContext
import org.antlr.v4.runtime.RuleContext
import org.antlr.v4.runtime.tree.ParseTreeProperty

private[instrumentor] object Util {
  def idOfDeclarator(ctx : CParser.DirectDeclaratorContext) : String =
    ctx match {
      case id : DeclaredIdentifierContext => id.getText();
      case paren : DeclaredParenthesesContext =>
        idOfDeclarator(paren.declarator().directDeclarator());
      case arr : DeclaredArrayContext =>
        idOfDeclarator(arr.directDeclarator());
      case funProto : DeclaredFunctionPrototypeContext =>
        idOfDeclarator(funProto.directDeclarator());
      case funDefn : DeclaredFunctionDefinitionContext =>
        idOfDeclarator(funDefn.directDeclarator());
    }

  def currentScopeForContext[T](ctx : RuleContext, scopes : ParseTreeProperty[Scope]) : Scope = {
    val scope = scopes.get(ctx);
    val parent = ctx.getParent();
    if (scope != null) {
      return scope;
    } else if (parent != null) {
      return currentScopeForContext[T](parent, scopes);
    } else {
      throw new IllegalStateException("Assumed to have a Scope by time reaches root node.");
    }
  }

  // For finding a common real type between two arithmetic types.
  // e.g. commonRealType("int", "float") = "float"
  //
  // For convenience, it is assumed only "type specifier" types are given.
  def commonRealType(t1 : String, t2 : String) : String = {
    val typeSpecs = Seq("char", "short", "int", "long", "float", "double", "long double");
    return typeSpecs(Seq(t1, t2).map(typeSpecs.indexOf).max);
  }

  def isIntType(ct : CType) : Boolean =
    ct match {
      case PrimitiveType(_, t) => Seq("char", "short", "int", "long").contains(t);
      case _ => false;
    }

  def isArithmeticType(ct : CType) : Boolean =
    ct match {
      case PrimitiveType(_, t) => Seq("char", "short", "int", "long", "float", "double", "long double").contains(t);
      case _ => false;
    }
}