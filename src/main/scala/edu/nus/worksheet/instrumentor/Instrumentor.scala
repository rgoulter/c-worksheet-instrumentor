package edu.nus.worksheet.instrumentor

import org.antlr.v4.runtime._
import org.antlr.v4.runtime.tree._
import org.stringtemplate.v4._
import scala.beans.BeanProperty
import scala.io.Source;
import scala.util.matching.Regex;


case class LineDirective(nonce : String = "") {
  def code(line : Int) : String = {
    val template = Instrumentor.constructionSTG.getInstanceOf("lineDirective");
    template.add("lineNum", line);
    template.add("nonce", nonce);
    return template.render();
  }

  // Regex has group for any STDOUT before the "LINE #",
  // as well as the directive's line number.
  def regex() : Regex =
    s"(.*)LINE$nonce (\\d+)".r
}

case class WorksheetDirective(nonce : String = "") {
  def code(output : String, printfArgs : Seq[String] = Seq()) : String = {
    val args = printfArgs.map { a => ", " + a }.mkString;
    s"""printf("WORKSHEET$nonce $output\\n"$args);""";
  }

  // Regex has group for the output to add to the regex.
  def regex() : Regex =
    s"WORKSHEET$nonce (.*)".r
}

case class FunctionEnterDirective(nonce : String = "") {
  def code() : String =
    s"""printf("FUNCTION$nonce ENTER\\n");""";

  def regex() : Regex =
    s"FUNCTION$nonce ENTER".r
}

case class FunctionReturnDirective(nonce : String = "") {
  def code() : String =
    s"""printf("FUNCTION$nonce RETURN\\n");""";

  def regex() : Regex =
    s"FUNCTION$nonce RETURN".r
}

/**
Tooler to augment the tokenstream by adding stuff before/after statements.
*/
class Instrumentor(val tokens : BufferedTokenStream,
                   stringCons : StringConstruction,
                   typeInfer : TypeInference,
                   nonce : String = "") extends CBaseListener {
  val rewriter = new TokenStreamRewriter(tokens);

  var blockLevel = 0;

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
    val startIdx = ctx.getStart().getTokenIndex();

    // Assumes presence of some token on a previous line.
    var t : Token = ctx.start;
    val tokStream = rewriter.getTokenStream();
    while (t.getTokenIndex() > 0 &&
           t.getTokenIndex() >= startIdx &&
           tokStream.get(t.getTokenIndex() - 1).getLine() >= line) {
      t = tokStream.get(t.getTokenIndex() - 1);
    }

    val indent = " " * t.getCharPositionInLine(); // assume no tabs
    rewriter.insertAfter(t, s"$str\n$indent");
  }

  def addLineAfter(ctx : ParserRuleContext, str : String) = {
    val line = ctx.start.getLine();
    val endIdx = ctx.getStop().getTokenIndex();

    // Assumes presence of some token on a next line.
    var t : Token = ctx.stop;
    val tokStream = rewriter.getTokenStream();
    while (t.getTokenIndex() + 1 < tokStream.size() &&
           t.getTokenIndex() + 1 <= endIdx + 1 &&
           tokStream.get(t.getTokenIndex() + 1).getLine() <= line) {
      t = tokStream.get(t.getTokenIndex() + 1);
    }

    // We can be cleverer about this.
    val indent = " " * ctx.start.getCharPositionInLine(); // assume no tabs
    rewriter.insertAfter(t, s"$indent$str\n");
  }

  private[Instrumentor] def generateInstrumentorPreamble() : String = {
    // It doesn't matter that #include occurs more than once.
    val preambleTemplate = Instrumentor.constructionSTG.getInstanceOf("preamble");
    return preambleTemplate.render();
  }

  override def enterCompilationUnit(ctx : CParser.CompilationUnitContext) {
    rewriter.insertBefore(ctx.getStart(), generateInstrumentorPreamble()) ;
  }

  // blockItem = declaration or statement
  override def enterBlockItem(ctx : CParser.BlockItemContext) {
  }

  override def exitBlockItem(ctx : CParser.BlockItemContext) {
    val ctxLine = ctx.start.getLine();
    val lineDirective = LineDirective(nonce);
    addLineBefore(ctx, lineDirective.code(ctxLine));
  }

  override def exitDeclaration(ctx : CParser.DeclarationContext) {
    if (ctx.getParent().isInstanceOf[CParser.BlockItemContext]) {
      val english = new GibberishPhase(tokens).visitDeclaration(ctx);
      val wsDirective = WorksheetDirective(nonce);
      addLineBefore(ctx, wsDirective.code(english));
    }
  }

  private[Instrumentor] def generateStringConstruction(ctype : CType, printPrefixStr : String = "") : String = {
    val buf : StrConsBuffer = StrConsBuffer.next();

    val declarationTemplate = Instrumentor.constructionSTG.getInstanceOf("declaration");
    declarationTemplate.add("buf", buf);
    val declareBufCode : String = declarationTemplate.render();

    val outputTemplate = Instrumentor.constructionSTG.getInstanceOf("output");
    outputTemplate.add("buf", buf);
    outputTemplate.add("T", ctype);
    val constructionCode = outputTemplate.render();

    val wsDirective = WorksheetDirective(nonce);
    val printCode = wsDirective.code(printPrefixStr + "%s", Seq(buf.ptr))

    val freeCode = s"free(${buf.ptr}); ${buf.ptr} = NULL;"; // INSTR CODE

    Seq(declareBufCode, constructionCode, printCode, freeCode).mkString("\n");
  }

  private[Instrumentor] def addStringConstructionFor(ctx : CParser.AssignmentExpressionContext) {
    if (ctx.unaryExpression() != null) { // Check which case it is.
      val theAssg = rewriter.getText(ctx.getSourceInterval());
      val unaryStr = rewriter.getText(ctx.unaryExpression().getSourceInterval());

      // Generate code to construct string.
      val output = stringCons.lookup(ctx, unaryStr) match {
        case Some(assgCType) => {
          generateStringConstruction(assgCType, s"${assgCType.id} = ");
        }
        case None => {
          println(s"Couldn't find CType for $unaryStr in $theAssg, Line ${ctx.start.getLine()}");
          s"// Couldn't find CType for $unaryStr in $theAssg";
        }
      }

      addLineAfter(ctx, output);
    }
  }

  override def exitExpressionStatement(ctx : CParser.ExpressionStatementContext) {
    if (ctx.expression() != null) {
      val expr = ctx.expression();
      val assgExpr = expr.assignmentExpression();

      val theAssg = rewriter.getText(assgExpr.getSourceInterval());

      if (assgExpr.unaryExpression() != null) {
        // assgExpr some kind of "lvalue = expr".
        addStringConstructionFor(assgExpr);
      } else {
        try {
          val exprType = typeInfer.visitAssignmentExpression(assgExpr);

          if (exprType != null) {
            val output = generateStringConstruction(exprType);
            addLineAfter(ctx, output);
          }
        } catch {
          case e : Throwable => ();
        }
      }
    }
  }

  override def enterFunctionDefinition(ctx : CParser.FunctionDefinitionContext) {
    val compoundStmt = ctx.compoundStatement();

    // Insert after {: print "ENTER FUNCTION"
    val startTok = compoundStmt.getStart();
    val enterCode = FunctionEnterDirective(nonce).code();
    rewriter.insertAfter(startTok, s"\n$enterCode\n")

    // Insert before }: print "RETURN FUNCTION"
    val stopTok = compoundStmt.getStop();
    val returnCode = FunctionReturnDirective(nonce).code();
    rewriter.insertBefore(stopTok, s"\n$returnCode\n")
  }

  override def exitJumpStatement(ctx : CParser.JumpStatementContext) {
    ctx.getChild(0).getText() match {
      // 'RETURN' directives are added around a `return` statement
      // so that the instrumentor can keep track of the current line.
      case "return" => {
        //   return e;
        // becomes
        //   { printf(WORKSHEET_RETURN); return e; }
        val code = FunctionReturnDirective(nonce).code();
        addLineBefore(ctx, s"{ $code");
        rewriter.insertAfter(ctx.getStop(), "}");
      }
    }
  }
}

object Instrumentor {
  private val constructionSTGResource = "edu/nus/worksheet/instrumentor/templates/constructs.stg";
  private val constructionSTGResourcesIS =
    getClass().getClassLoader().getResourceAsStream(constructionSTGResource);
  private[instrumentor] val constructionSTG =
    new STGroupString(Source.fromInputStream(constructionSTGResourcesIS).mkString);


  private[instrumentor] def renderTemplateWithValues(templateName : String, args : Array[(String, Any)]) : String = {
    val template = Instrumentor.constructionSTG.getInstanceOf(templateName);
    for ((k,v) <- args) {
      template.add(k, v);
    }
    return template.render();
  }

  def instrument(inputProgram : String, nonce : String = "") : String = {
    val input = new ANTLRInputStream(inputProgram);
    val lexer = new CLexer(input);
    val tokens = new CommonTokenStream(lexer);
    val parser = new CParser(tokens);

    val tree = parser.compilationUnit(); // entry rule for parser

    val walker = new ParseTreeWalker();

    val defineScopesPhase = new DefineScopesPhase[CType]();
    walker.walk(defineScopesPhase, tree);
    val scopes = defineScopesPhase.scopes;

    val strCons = new StringConstruction(tokens, scopes);
    walker.walk(strCons, tree);

    val typeInfer = new TypeInference(strCons);
    val tooler = new Instrumentor(tokens, strCons, typeInfer, nonce);
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