package edu.nus.worksheet.instrumentor

import org.antlr.v4.runtime._
import org.antlr.v4.runtime.tree._
import org.stringtemplate.v4._
import scala.beans.BeanProperty
import scala.io.Source;
import scala.collection.mutable;
import scala.util.matching.Regex;
import edu.nus.worksheet.instrumentor.CTypeToDeclaration.declarationOf;


class Directive(nonce : String = "") {
  // in a printf, abc => \"abc\"
  def wrapString(s : String) : String =
    s"""\\"$s\\""""

  // Render with sequence of (key, value, args) tuples
  def renderDirectiveCode(kvPairs : Seq[(String, String, Seq[String])]) : String = {
    val kvs = kvPairs.map({ case (k, v, _) => (k, v) });
    val args = kvPairs.map({ case (_, _, args) => args }).flatten;
    renderDirectiveCode(kvs, args);
  }

  def renderDirectiveCode(k : String, v : String, args : Seq[String] = Seq()) : String =
    renderDirectiveCode(Seq((k, v)), args);

  // Render with sequence of (key, value) tuples, and args
  def renderDirectiveCode(kvPairs : Seq[(String, String)], args : Seq[String]) : String = {
    val printfTempl = Instrumentor.constructionSTG.getInstanceOf("wsDirectivePrintf");

    printfTempl.add("nonce", nonce);
    printfTempl.add("keys", kvPairs.map({case (k, _) => k}).toArray);
    printfTempl.add("values", kvPairs.map({case (_, v) => v}).toArray);
    printfTempl.add("args", args.toArray);

    printfTempl.render();
  }
}


case class LineDirective(nonce : String = "") extends Directive(nonce) {
  def code(line : Int, scopeName : String, blockIterName : String = "blockIteration") : String =
    renderDirectiveCode(Seq(("line", line.toString(), Seq()),
                            ("scopeName", wrapString(scopeName), Seq()),
                            ("blockIteration", "%d", Seq(blockIterName))));

  // Regex has group for any STDOUT before the "LINE #",
  // as well as the directive's line number.
  def regex() : Regex =
    s"""(.*)WORKSHEET$nonce \\{ "line": (\\d+), "scopeName": "(.*)", "blockIteration": (\\d+) \\}""".r
}

case class WorksheetDirective(nonce : String = "") extends Directive(nonce) {
  def code(output : String, printfArgs : Seq[String] = Seq()) : String =
    renderDirectiveCode("output", wrapString(output), printfArgs);

  // Regex has group for the output to add to the regex.
  def regex() : Regex =
    s"""WORKSHEET$nonce \\{ "output": "(.*)" \\}""".r
}

case class FunctionEnterDirective(nonce : String = "") extends Directive(nonce) {
  def code() : String =
    renderDirectiveCode("function", wrapString("enter"));

  def regex() : Regex =
    s"""WORKSHEET$nonce \\{ "function": "enter" \\}""".r
}

case class FunctionReturnDirective(nonce : String = "") extends Directive(nonce) {
  def code() : String =
    renderDirectiveCode("function", wrapString("return"));

  def regex() : Regex =
    s"""WORKSHEET$nonce \\{ "function": "return" \\}""".r
}

/**
Tooler to augment the tokenstream by adding stuff before/after statements.
*/
class Instrumentor(val tokens : BufferedTokenStream,
                   stringCons : StringConstruction,
                   typeInfer : TypeInference,
                   nonce : String = "") extends CBaseListener {
  val rewriter = new TokenStreamRewriter(tokens);

  val blockFilters : mutable.Map[String, Int => Boolean] = mutable.HashMap();

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

  private[Instrumentor] def segfaultGuardCode() : String = {
    val template = Instrumentor.constructionSTG.getInstanceOf("segfaultGuard");
    template.render();
  }

  override def exitBlockItem(ctx : CParser.BlockItemContext) {
    val ctxLine = ctx.start.getLine();
    val blockName = stringCons.scopeOfContext(ctx).scopeName;

    val lineDirective = LineDirective(nonce);
    addLineBefore(ctx, lineDirective.code(ctxLine, blockName));
    addLineBefore(ctx, segfaultGuardCode);
  }

  override def exitDeclaration(ctx : CParser.DeclarationContext) {
    if (ctx.getParent().isInstanceOf[CParser.BlockItemContext]) {
      val english = new GibberishPhase(tokens).visitDeclaration(ctx);
      val wsDirective = WorksheetDirective(nonce);

      // We only want to have declarations printed once.
      val execOnlyOnce = s"""{
  static int hasExecuted = 0;
  if (!hasExecuted) {
    ${wsDirective.code(english)}
    hasExecuted = 1;
  }
}""";
      addLineBefore(ctx, execOnlyOnce);
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
            if (!exprType.isInstanceOf[ArrayType]) {
              // To ensure no side-effect expressions,
              // want to transform like:
              //   E
              // becomes
              // { T res =
              //   E;
              //   output(res);
              // }

              val resId = "wsExprResult";
              val decln = declarationOf(exprType, resId);
              val output = generateStringConstruction(exprType.changeId(resId));

              addLineBefore(ctx, s"{ $decln; $resId =")

              // "AddLine" prepends the output, unfortunately.
              addLineAfter(ctx, "}");
              addLineAfter(ctx, output);
            } else {
              // This special case is needed because the above does "T tmp = E;",
              // but when E is an array (identifier), this doesn't work.
              // -- It's still possible, therefore, that the worksheet will evaluate expression
              // statements with side effects (e.g. `*(ptrToArr++)`),
              // but this is "less wrong" until we solve this.
              val output = generateStringConstruction(exprType);

              addLineAfter(ctx, output);
            }
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

  private[Instrumentor] def checkForFilterBlockCommentInBlock(ctx : CParser.CompoundStatementContext) {
    // We're looking for the FIRST comment after a block,
    // to see if it contains a message to 'filter' to an iteration.

    var idx = ctx.getStart.getTokenIndex() + 1;
    def tokenAt(i : Int) = rewriter.getTokenStream().get(i);

    // Skip all the whitespace.
    while (tokenAt(idx).getChannel() == CLexer.WHITESPACE) {
      idx += 1;
    }

    // Next token will either be a comment we might want,
    // or C statement we can ignore.
    if (tokenAt(idx).getChannel() == CLexer.COMMENT) {
      val tok = tokenAt(idx);
      val tokText = tok.getText().trim();

      val SingleLineCmt = "// worksheet filter iteration == (\\d+)".r;
      val MultiLineCmt = "/\\* worksheet filter iteration == (\\d+) \\*/".r;

      val currentBlockName = stringCons.scopeOfContext(ctx).scopeName;

      tokText match {
        case SingleLineCmt(d) => blockFilters += currentBlockName -> { iter => iter == d.toInt; };
        case MultiLineCmt(d) => blockFilters += currentBlockName -> { iter => iter == d.toInt; };
        case _ => ();
      }
    }
  }

  override def enterCompoundStatement(ctx : CParser.CompoundStatementContext) = {
    val startTok = ctx.getStart();
    val iterationVarName = "blockIteration";
    rewriter.insertBefore(startTok, s"{ /*CTR*/ static int $iterationVarName = -1; $iterationVarName += 1; ");


    val stopTok = ctx.getStop();
    rewriter.insertAfter(stopTok, " /*CTR*/ }");

    checkForFilterBlockCommentInBlock(ctx);
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

  def instrumentorFor(inputProgram : String, nonce : String = "") : Instrumentor = {
    val input = new ANTLRInputStream(inputProgram);
    val lexer = new CLexer(input);
    val tokens = new CommonTokenStream(lexer);
    val parser = new CParser(tokens);

    val tree = parser.compilationUnit(); // entry rule for parser

    val walker = new ParseTreeWalker();

    val defineScopesPhase = new DefineScopesPhase();
    walker.walk(defineScopesPhase, tree);
    val scopes = defineScopesPhase.scopes;

    val strCons = new StringConstruction(scopes);
    walker.walk(strCons, tree);

    val typeInfer = new TypeInference(strCons);
    val tooler = new Instrumentor(tokens, strCons, typeInfer, nonce);
    walker.walk(tooler, tree);

    return tooler;
  }

  def instrument(inputProgram : String, nonce : String = "") : String = {
    val tooler = instrumentorFor(inputProgram, nonce);
    tooler.rewriter.getText();
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
    // worksheet filter iteration == 3
  }
  // Don't want to see *this* comment, though. THREE
  for (y = 0; y < 3; y = y + 1) {
    // worksheet filter iteration == 7
  }
}
""";
    println(instrument(inputProgram));
  }
}
