package edu.nus.worksheet.instrumentor

import org.antlr.v4.runtime._
import org.antlr.v4.runtime.tree._
import org.stringtemplate.v4._
import scala.beans.BeanProperty
import scala.io.Source;


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
class Instrumentor(val tokens : BufferedTokenStream, stringCons : StringConstruction) extends CBaseListener {
  val rewriter = new TokenStreamRewriter(tokens);

  var blockLevel = 0;
  
  val constructionSTGResource = "edu/nus/worksheet/instrumentor/templates/constructs.stg";
  val constructionSTGResourcesIS =
    getClass().getClassLoader().getResourceAsStream(constructionSTGResource);
  val constructionSTG =
    new STGroupString(Source.fromInputStream(constructionSTGResourcesIS).mkString);
  
  class StrConsBuffer(@BeanProperty val ptr : String,
                      @BeanProperty val offset : String,
                      @BeanProperty val len : String);

  object StrConsBuffer {
    var idx = -1;
    
    def next() : StrConsBuffer = {
      idx += 1;
      return new StrConsBuffer(s"res$idx", s"offset_res$idx", s"len_res$idx");
    }
  }

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
    rewriter.insertAfter(t, s"$str\n$indent");
  }
  
  def addLineAfter(ctx : ParserRuleContext, str : String) = {
    val line = ctx.start.getLine();

    // Assumes presence of some token on a next line.
    var t : Token = ctx.stop;
    val tokStream = rewriter.getTokenStream();
    while (tokStream.get(t.getTokenIndex() + 1).getLine() <= line) {
      t = tokStream.get(t.getTokenIndex() + 1);
    }

    // We can be cleverer about this.
    val indent = " " * ctx.start.getCharPositionInLine(); // assume no tabs
    rewriter.insertAfter(t, s"$indent$str\n");
  }
  
  private[Instrumentor] def generateInstrumentorPreamble() : String = {
    // It doesn't matter that #include occurs more than once.
    // (Should this be in our ST4 Template Group?).
    return """#include <stdio.h>
#include <stdlib.h>
#include <string.h>
"""
  }
  
  override def enterCompilationUnit(ctx : CParser.CompilationUnitContext) {
    rewriter.insertBefore(ctx.getStart(), generateInstrumentorPreamble()) ;
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
  
  private[Instrumentor] def generateStringConstruction(ctype : CType) : String = {
    val buf : StrConsBuffer = StrConsBuffer.next();

    val declarationTemplate = constructionSTG.getInstanceOf("declaration");
    declarationTemplate.add("buf", buf);
    val declareBufCode : String = declarationTemplate.render();
    
    val outputTemplate = constructionSTG.getInstanceOf("output");
    outputTemplate.add("buf", buf);
    outputTemplate.add("T", ctype);
    val constructionCode = outputTemplate.render();
    
    val printCode = s"""printf("WORKSHEET ${ctype.id} = %s\\n", ${buf.ptr});"""
    
    val freeCode = s"free(${buf.ptr}); ${buf.ptr} = NULL;";
    
    Seq(declareBufCode, constructionCode, printCode, freeCode).mkString("\n");
  }
  
  override def exitAssignmentExpression(ctx : CParser.AssignmentExpressionContext) {
    if (ctx.unaryExpression() != null) { // Check which case it is.
      val theAssg = rewriter.getText(ctx.getSourceInterval());
      val unaryStr = rewriter.getText(ctx.unaryExpression().getSourceInterval());
      
      // Generate code to construct string.
      stringCons.lookup(unaryStr) match {
        case Some(assgCType) => {
          val output = generateStringConstruction(assgCType);
          
          addLineAfter(ctx, output);
        }
        case None => {
          val output = s"// Couldn't find CType for $unaryStr in $theAssg";
          println(s"Couldn't find CType for $unaryStr in $theAssg, Line ${ctx.start.getLine()}");
          addLineAfter(ctx, output);
        }
      }
      
    }
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
    val strCons = new StringConstruction(tokens);
    walker.walk(strCons, tree);
    val tooler = new Instrumentor(tokens, strCons);
    walker.walk(tooler, tree);

    return tooler.rewriter.getText();
  }
  
  def main(args : Array[String]) : Unit = {
    val inputProgram = """#include <stdio.h>

int main() {
  int x = 3;
  int y;

  x = 3;
  y = 0;

  struct MyPoint { int x; int y; };
  struct MyPoint p = { 3, 5 };
  struct MyPoint q = { 3, 5 };
  p = q;

  printf("this is line 7 (starting from 1)");

  for (int i = 0; i < 5; i++) {
  }
  for (y = 0; y < 3; y = y + 1) {
  }
}
""";
    println(instrument(inputProgram));
  }
}