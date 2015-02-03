package edu.nus.worksheet.instrumentor

import org.antlr.v4.runtime._
import org.antlr.v4.runtime.tree._

case class LineDirective(line : Int) {
  def code() : String =
    s"""printf("LINE $line\\n");""" 
}

case class WorksheetDirective(output : String) {
  def code() : String =
    s"""printf("WORKSHEET $output\\n");""" 
}

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
  
  def addBefore(ctx : ParserRuleContext, str : String) = {
    val indent = " " * ctx.start.getCharPositionInLine(); // assume no tabs
    rewriter.insertBefore(ctx.start, s"$str\n$indent");
  }
  
  def addAfter(ctx : ParserRuleContext, str : String) = {
    val indent = " " * ctx.start.getCharPositionInLine(); // assume no tabs
    rewriter.insertAfter(ctx.stop, s"\n$indent$str");
  }
  
  def addLineBefore(ctx : ParserRuleContext, str : String) = {
    val line = ctx.start.getLine();

    // Assumes presence of some token on a previous line.
    var t : Token = ctx.start;
    val tokStream = rewriter.getTokenStream();
    while (t.getTokenIndex() > 0 &&
           tokStream.get(t.getTokenIndex() - 1).getLine() >= line) {
      t = tokStream.get(t.getTokenIndex() - 1);
    }

    val indent = " " * t.getCharPositionInLine(); // assume no tabs
    rewriter.insertBefore(t, s"$str\n$indent");
  }
  
  // blockItem = declaration or statement
  override def enterBlockItem(ctx : CParser.BlockItemContext) {
  }

  override def exitBlockItem(ctx : CParser.BlockItemContext) {
    val ctxLine = ctx.start.getLine();
    val lineDirective = LineDirective(ctxLine);
    addLineBefore(ctx, lineDirective.code());
  }
  
  override def exitDeclaration(ctx : CParser.DeclarationContext) {
    val english = new GibberishPhase(tokens).visitDeclaration(ctx);
    val wsDirective = WorksheetDirective(english);
    addLineBefore(ctx, wsDirective.code());
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

  for (int i = 0; i < 5; i++) {
  }
}
""";
    println(instrument(inputProgram));
  }
}