package edu.nus.worksheet.instrumentor

import edu.nus.worksheet.instrumentor.CParser.FunctionDefinitionContext
import edu.nus.worksheet.instrumentor.CParser.DeclaredFunctionDefinitionContext
import edu.nus.worksheet.instrumentor.CParser.DeclaredIdentifierContext
import edu.nus.worksheet.instrumentor.CParser.DeclaredParenthesesContext
import edu.nus.worksheet.instrumentor.CParser.DeclaredArrayContext
import edu.nus.worksheet.instrumentor.CParser.DeclaredFunctionPrototypeContext

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
}