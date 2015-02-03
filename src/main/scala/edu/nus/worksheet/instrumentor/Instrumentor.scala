package edu.nus.worksheet.instrumentor

import org.antlr.v4.runtime._
import org.antlr.v4.runtime.tree._

/**
Tooler to augment the tokenstream by adding stuff before/after statements.
*/
class Instrumentor(val tokens : BufferedTokenStream) extends CBaseListener {
  val rewriter = new TokenStreamRewriter(tokens);

  var blockLevel = 0;

  override def enterCompoundStatement(ctx : CParser.CompoundStatementContext) =
    blockLevel += 1;

  override def exitCompoundStatement(ctx : CParser.CompoundStatementContext) =
    blockLevel -= 1;
  
  // blockItem = declaration or statement
  override def enterBlockItem(ctx : CParser.BlockItemContext) {
    val ctxLine = ctx.start.getLine();
    val indent = " " * ctx.start.getCharPositionInLine(); // assume no tabs
    val outputLine = s"""printf("LINE $ctxLine\\n");""" // printf("LINE #");

    rewriter.insertBefore(ctx.start, s"$outputLine\n$indent");
  }

  override def exitBlockItem(ctx : CParser.BlockItemContext) {
    
  }
  
  override def exitDeclaration(ctx : CParser.DeclarationContext) {
    val indent = " " * ctx.start.getCharPositionInLine(); // assume no tabs
    rewriter.insertAfter(ctx.stop, s"\n$indent// DECLARATION\n");
  }
}

object Instrumentor {
  def instrument(inputProgram : String) : String = {
    val input = new ANTLRInputStream(inputProgram);
    val lexer = new CLexer(input);
    val tokens = new CommonTokenStream(lexer);
    val parser = new CParser(tokens);

    val tree = parser.compilationUnit(); // entry rule for parser

    val walker = new ParseTreeWalker();
    val tooler = new Instrumentor(tokens);
    walker.walk(tooler, tree);

    return tooler.rewriter.getText();
  }
  
  def main(args : Array[String]) : Unit = {
    val inputProgram = """#include <stdio.h>

int main() {
  int x = 3;
  int y;

  printf("this is line 7 (starting from 1)");
}
""";
    println(instrument(inputProgram));
  }
}