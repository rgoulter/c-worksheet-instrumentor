// CDecl Proof of Concept in Scala.
// perhaps move to its own project in Java.
package edu.nus.worksheet.instrumentor;

import org.antlr.v4.runtime.tree.ParseTreeWalker;
import org.antlr.v4.runtime.{BufferedTokenStream, TokenStreamRewriter};

import edu.nus.worksheet.instrumentor.Util;
import Util.getANTLRLexerTokensParserFor;

// Pass through a "gibberish" (C) declaration, come up with English terms.
// Since we want a String from each result, a Visitor is appropriate;
// (we can then also control *what* we visit, so don't double count).
class GibberishPhase(val tokens: BufferedTokenStream)
    extends CBaseVisitor[String] {
  val rewriter = new TokenStreamRewriter(tokens);

  override def visitDeclaration(ctx: CParser.DeclarationContext): String = {
    val declSpecs = visit(ctx.declarationSpecifiers());
    val theRest =
      if ctx.maybeInitDeclaratorList().initDeclaratorList() != null then
        visit(
          ctx.maybeInitDeclaratorList().initDeclaratorList()
        ); // assume only one variable for now.
      else "";

    return theRest + declSpecs;
  }

  override def visitDeclarationSpecifiers(
      ctx: CParser.DeclarationSpecifiersContext
  ): String = {
    rewriter.getText(ctx.getSourceInterval());
  }

  override def visitDeclarationSpecifiers2(
      ctx: CParser.DeclarationSpecifiers2Context
  ): String = {
    rewriter.getText(ctx.getSourceInterval());
  }

  override def visitPointer(ctx: CParser.PointerContext): String = {
    val typeQuals =
      if ctx.typeQualifierList() != null then
        rewriter.getText(ctx.typeQualifierList().getSourceInterval()) + " "
      else "";
    return typeQuals + "pointer to " + (if ctx.pointer() != null then
                                          visit(ctx.pointer())
                                        else "");
  }

  // because "int x" fucks up, and so "int x = 3" fucks up also, need this:
  override def visitInitDeclarator(ctx: CParser.InitDeclaratorContext) =
    visit(ctx.declarator());

  override def visitDeclaredParentheses(
      ctx: CParser.DeclaredParenthesesContext
  ): String =
    visit(ctx.declarator());

  override def visitDeclarator(ctx: CParser.DeclaratorContext): String = {
    val directDecl = visit(ctx.directDeclarator());
    return directDecl + (if ctx.pointer() != null then visit(ctx.pointer())
                         else "");
  }

  override def visitDeclaredIdentifier(
      ctx: CParser.DeclaredIdentifierContext
  ): String = {
    rewriter.getText(ctx.getSourceInterval()) + " is ";
  }

  // Ouch. FIXME
//  override def visitDirectAbstractDeclarator(ctx : CParser.DirectAbstractDeclaratorContext) : String =
//    rewriter.getText(ctx.getSourceInterval()) + " ";

  override def visitDeclaredArray(ctx: CParser.DeclaredArrayContext): String = {
    // assignmentExpression not guaranteed; may be '*' in func. arg.
    // or just not there, e.g. in "int arr[] = { 1, 2 };"
    val directDecl = visit(ctx.directDeclarator());
    val arrSizeExpr =
      if ctx.assignmentExpression() != null then
        rewriter.getText(
          ctx.assignmentExpression().getSourceInterval()
        ) + " "; // visit?
      else "";
    return directDecl + "array " + arrSizeExpr + "of ";
  }

  override def visitDeclaredFunctionPrototype(
      ctx: CParser.DeclaredFunctionPrototypeContext
  ): String = {
    val directDecl = visit(ctx.directDeclarator());

    val params =
      if ctx.parameterTypeList() != null then visit(ctx.parameterTypeList())
      else "";

    return directDecl + "function (" + params + ") returning ";
  }

  override def visitParameterList(ctx: CParser.ParameterListContext): String = {
    return (if ctx.parameterList() != null then
              visit(ctx.parameterList()) + ", "
            else "") +
      visit(ctx.parameterDeclaration());
  }

  override def visitParameterDeclaration(
      ctx: CParser.ParameterDeclarationContext
  ): String = {
    val abstrDecl =
      if ctx.abstractDeclarator() != null then visit(ctx.abstractDeclarator())
      else "";
    // need to figure out which rule it is.
    val declSpecs = visit(
      if ctx.declarationSpecifiers() != null then ctx.declarationSpecifiers()
      else ctx.declarationSpecifiers2()
    );
    return abstrDecl + declSpecs;
  }
}

object CDecl {
  def gibberishToEnglish(gibberish: String): String = {
    val (lexer, tokens, parser) = getANTLRLexerTokensParserFor(gibberish);

    val tree = parser.declaration(); // entry rule for parser

    val walker = new ParseTreeWalker();
    val tooler = new GibberishPhase(tokens);
    tooler.visit(tree);
  }
}
