package edu.nus.worksheet.instrumentor;

import org.antlr.v4.runtime.tree.*;
import org.antlr.v4.runtime.*;

private[instrumentor] class StdinExtractor extends InlineStdinBaseListener {
  var stdinLines = Seq[String]();

  override def exitStdin(ctx: InlineStdinParser.StdinContext): Unit = {
    stdinLines = stdinLines :+ ctx.getText().trim();
  }
}

object StdinMarkup {
  def extractFromSource(src: String): Seq[String] = {
    val input = new ANTLRInputStream(src);
    val lexer = new InlineStdinLexer(input);
    val tokens = new CommonTokenStream(lexer);
    val parser = new InlineStdinParser(tokens);

    // Suppress Error Listeners.
    // So long as things work when they should..
    lexer.removeErrorListeners();
    parser.removeErrorListeners();

    val tree = parser.source();

    val extractor = new StdinExtractor();
    val walker = new ParseTreeWalker();
    walker.walk(extractor, tree);

    return extractor.stdinLines;
  }
}
