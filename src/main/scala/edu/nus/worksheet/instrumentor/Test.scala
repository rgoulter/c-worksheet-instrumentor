package edu.nus.worksheet.instrumentor

import org.antlr.v4.runtime._
import org.antlr.v4.runtime.tree._

object Test {
  def main(args : Array[String]) : Unit = {
    val input = new ANTLRInputStream(System.in);
    val lexer = new CLexer(input);
    val tokens = new CommonTokenStream(lexer);
    val parser = new CParser(tokens);

    val tree = parser.compilationUnit(); // entry rule for parser

    val walker = new ParseTreeWalker();
    val tooler = new Tooler(tokens);
    walker.walk(tooler, tree);
    println(tooler.rewriter.getText());
  }
}
