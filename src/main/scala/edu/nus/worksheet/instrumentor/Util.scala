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

  def currentScopeForContext[T](ctx : RuleContext, scopes : ParseTreeProperty[Scope[T]]) : Scope[T] = {
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
}