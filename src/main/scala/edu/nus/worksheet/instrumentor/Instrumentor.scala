package edu.nus.worksheet.instrumentor

import org.antlr.v4.runtime._
import org.antlr.v4.runtime.tree._
import org.stringtemplate.v4._
import scala.beans.BeanProperty
import scala.io.Source
import scala.collection.JavaConversions._;
import scala.collection.mutable
import scala.util.matching.Regex
import edu.nus.worksheet.instrumentor.Util.currentScopeForContext;
import edu.nus.worksheet.instrumentor.Util.getANTLRLexerTokensParserFor;
import edu.nus.worksheet.instrumentor.Util.lookup;
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
  // Sends a simple JSON, e.g. { "output" : "some stuff" },
  // `printfArgs` is used in case the given `output` needs to be constructed at runtime.
  // So, e.g. { "output": "value is %d" } can have printfArg Seq("x"), to output value of x.
  def code(output : String, kind : String = "output", printfArgs : Seq[String] = Seq()) : String =
    renderDirectiveCode(kind, wrapString(output), printfArgs);

  // Regex has group for the output to add to the regex.
  def regex(kind : String = "output") : Regex =
    s"""(.*)WORKSHEET$nonce \\{ "$kind": "(.*)" \\}""".r
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
                   scopes : ParseTreeProperty[Scope],
                   typeInfer : TypeInference,
                   nonce : String = "") extends CBaseListener {
  val rewriter = new TokenStreamRewriter(tokens);

  def getInstrumentedProgram() : String =
    rewriter.getText();

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

    // Need to add "func ptr lookup" function at the end,
    // Otherwise might accidentally forward-reference a function.

    // (This needs to be done here, rather than in `exitComp..`, because
    //  compoundStatement has rewrites which interfere with this).

    val fpLookupTemplate = Instrumentor.constructionSTG.getInstanceOf("lookupFuncPointer");
    val listOfFunctionNames = getFunctionsNamesInScope(ctx);
    fpLookupTemplate.add("functionNames", listOfFunctionNames.toArray);
    val code = "\n\n" + fpLookupTemplate.render();

    rewriter.insertAfter(ctx.getStop(), code) ;
  }

  private[Instrumentor] def functionSymsOfScope(scope : Scope) : Iterable[FunctionType] =
    scope.symbols.values.map(_ match {
      case ft : FunctionType => Some(ft);
      case _ => None;
    }).flatten

  private[Instrumentor] def allFunctionSymsInScope(scope : Scope) : Iterable[FunctionType] =
    scope.enclosingScope match {
      case Some(parent) =>
        functionSymsOfScope(scope) ++: allFunctionSymsInScope(parent);
      case None =>
        functionSymsOfScope(scope);
    }

  private[Instrumentor] def getFunctionsNamesInScope(ctx : ParserRuleContext) : Iterable[String] = {
    val scope = currentScopeForContext(ctx, scopes);

    // Functions (e.g. from stdio.h) which start with `_`
    // don't work as identifiers in the eq. expression for some reason.
    allFunctionSymsInScope(scope).map(_.getId).filterNot(_.startsWith("_"));
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
    val blockName = currentScopeForContext(ctx, scopes).scopeName;
    val iterationVarName = blockIterationIdentifierFor(ctx);

    val lineDirective = LineDirective(nonce);
    addLineBefore(ctx, lineDirective.code(ctxLine, blockName, iterationVarName));
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
    val printCode = wsDirective.code(printPrefixStr + "%s", kind = "exprResult", printfArgs = Seq(buf.ptr))

    val freeCode = s"free(${buf.ptr}); ${buf.ptr} = NULL;"; // INSTR CODE

    Seq(declareBufCode, constructionCode, printCode, freeCode).mkString("\n");
  }

  private[Instrumentor] def addStringConstructionFor(ctx : CParser.AssgExprContext) {
    val theAssg = rewriter.getText(ctx.getSourceInterval());
    val unaryStr = ctx.unaryExpression().getText();

    // Generate code to construct string.
    val output = try {
      val assgCType = typeInfer.visit(ctx.unaryExpression());

      if (assgCType != null) {
        generateStringConstruction(assgCType, s"${assgCType.getId} = ");
      } else {
        s"// Couldn't find CType for $unaryStr in $theAssg";
      }
    } catch {
      case e : Throwable => s"// Couldn't find CType for $unaryStr in $theAssg";
    }

    // Add { } braces.
    addLineBefore(ctx, "{ ");
    // "AddLine" prepends the output, unfortunately.
    addLineAfter(ctx, " }");
    addLineAfter(ctx, output);
  }

  private[Instrumentor] def addStringConstructionFor(ctx : CParser.ExpressionStatementContext, assgCtx : CParser.AssignmentExpressionContext) {
    assgCtx match {
      case assgExprCtx : CParser.AssgExprContext =>
        addStringConstructionFor(assgExprCtx);
      case assgFallCtx : CParser.AssgFallthroughContext => {
        try {
          val exprType = typeInfer.visit(assgFallCtx);

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

  override def exitExpressionStatement(ctx : CParser.ExpressionStatementContext) =
    if (ctx.expression() != null) {
      ctx.expression() match {
        case fallthrough : CParser.ExprFallthroughContext =>
          addStringConstructionFor(ctx, fallthrough.assignmentExpression());
        case commaExprCtx : CParser.CommaExprContext =>
          addStringConstructionFor(ctx, commaExprCtx.assignmentExpression());
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
      // There are other jump statements, to be ignored.
      case _ => ();
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

      val currentBlockName = currentScopeForContext(ctx, scopes).scopeName;

      tokText match {
        case SingleLineCmt(d) => blockFilters += currentBlockName -> { iter => iter == d.toInt; };
        case MultiLineCmt(d) => blockFilters += currentBlockName -> { iter => iter == d.toInt; };
        case _ => ();
      }
    }
  }

  private[Instrumentor] def blockIterationIdentifierFor(ctx : ParserRuleContext) : String = {
    val blockName = currentScopeForContext(ctx, scopes).scopeName;
    s"__ws_blockIterationFor_$blockName";
  }

  override def enterCompoundStatement(ctx : CParser.CompoundStatementContext) = {
    val startTok = ctx.getStart();
    val iterationVarName = blockIterationIdentifierFor(ctx);
    rewriter.insertBefore(startTok, s"""{ /*CTR*/ static int $iterationVarName = -1;
  $iterationVarName += 1;
  if ($iterationVarName > WORKSHEET_MAX_ITERATIONS) {
    printf("\t[max iterations exceeded]\\n");
    exit(EXIT_SUCCESS);
  }
""");


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
    val (lexer, tokens, parser) = getANTLRLexerTokensParserFor(inputProgram);

    val tree = parser.compilationUnit(); // entry rule for parser

    val walker = new ParseTreeWalker();

    val defineScopesPhase = new DefineScopesPhase();
    walker.walk(defineScopesPhase, tree);
    val scopes = defineScopesPhase.scopes;

    // Add Types from headers
    val headers = StringConstruction.getIncludeHeadersOf(inputProgram);
    for (hdr <- headers) {
      HeaderUtils.addTypedefsOfHeaderToScope(hdr, defineScopesPhase.globals);
      HeaderUtils.addSymbolsOfHeaderToScope(hdr, defineScopesPhase.globals);
    }

    val strCons = new StringConstruction(scopes);
    walker.walk(strCons, tree);

    // Need to clean up any forward declarations.
    defineScopesPhase.allScopes.foreach(_.flattenForwardDeclarations());


    val ctypeFromDecl = new CTypeFromDeclaration(scopes);
    val typeInfer = new TypeInference(scopes, ctypeFromDecl);

    val tooler = new Instrumentor(tokens, scopes, typeInfer, nonce);
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
