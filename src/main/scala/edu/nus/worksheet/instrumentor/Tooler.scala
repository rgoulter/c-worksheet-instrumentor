package edu.nus.worksheet.instrumentor

import org.antlr.v4.runtime._

/**
Tooler to augment the tokenstream by adding stuff before/after statements.
*/
class Tooler(val tokens : BufferedTokenStream) extends CBaseListener {
  val rewriter = new TokenStreamRewriter(tokens);

  override def enterExpressionStatement(ctx : CParser.ExpressionStatementContext) = {
    // Add some comment before.
    rewriter.insertBefore(ctx.start, " /*before*/ ");
    rewriter.insertAfter(ctx.stop, " /*after*/ ");
  }

  override def exitExpressionStatement(ctx : CParser.ExpressionStatementContext) = {
    // Add some comment after.
  }
}
