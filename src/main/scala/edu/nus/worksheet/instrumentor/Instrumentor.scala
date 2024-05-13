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
    // `s` may be multiline; so wrap each line with \" \"
    s.linesIterator.map(str => s"""\\"$str\\"""").mkString(" ");

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

  // Because Instrumentor makes several re-writes, sometimes for the same
  // startToken (e.g. assignmentExpression and blockItem),
  // it's probably easier to manage rewrites if we 'stage' rewrites.
  //
  // For each context, store a string of 'before', 'after'.
  val rewrites = mutable.Map[Token, (String, String)]();

  def getInstrumentedProgram() : String = {
    // Could sort tokens using .getTokenIndex(), if need be.
    for (tok <- rewrites.keys) {
      rewrites.get(tok) match {
        case Some((l, r)) => {
          rewriter.insertBefore(tok, l);
          rewriter.insertAfter(tok, r);
        }
        case _ => ();
      }
    }
    rewriter.getText();
  }

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

  private[Instrumentor] def generateInstrumentorPreamble() : String = {
    // It doesn't matter that #include occurs more than once.
    val preambleTemplate = Instrumentor.constructionSTG.getInstanceOf("preamble");
    return preambleTemplate.render();
  }

  override def enterCompilationUnit(ctx : CParser.CompilationUnitContext) {
    rewrites.put(ctx.getStart(), (generateInstrumentorPreamble() + "\n\n", ""));

    // Need to add "func ptr lookup" function at the end,
    // Otherwise might accidentally forward-reference a function.

    val fpLookupTemplate = Instrumentor.constructionSTG.getInstanceOf("lookupFuncPointer");
    val listOfFunctionNames = getFunctionsNamesInScope(ctx);
    fpLookupTemplate.add("functionNames", listOfFunctionNames.toArray);
    val code = "\n\n" + fpLookupTemplate.render();

    rewrites.put(ctx.getStop(), ("", code));
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

  private[Instrumentor] def addLineDirectiveTo(ctx : ParserRuleContext) {
    val ctxLine = ctx.start.getLine();
    val blockName = currentScopeForContext(ctx, scopes).scopeName;
    val iterationVarName = blockIterationIdentifierFor(ctx);

    val lineDirective = LineDirective(nonce);
    val lineDirCode = lineDirective.code(ctxLine, blockName, iterationVarName);

    val (l, r) = rewrites.getOrElse(ctx.getStart(), ("", ""));
    rewrites.put(ctx.getStart(), (lineDirCode + "\n" + segfaultGuardCode + "\n" + l, r));
  }

  override def exitBlockItem(ctx : CParser.BlockItemContext) {
    addLineDirectiveTo(ctx);
  }

  override def exitDeclaration(ctx : CParser.DeclarationContext) {
    if (ctx.getParent().isInstanceOf[CParser.BlockItemContext]) {
      val english = new GibberishPhase(tokens).visitDeclaration(ctx);
      val wsDirective = WorksheetDirective(nonce);

      // We only want to have declarations printed once.
      val execOnlyOnce = s"""{/*Decl*/
  static int hasExecuted = 0;
  if (!hasExecuted) {
    ${wsDirective.code(english)}
    hasExecuted = 1;
  }
/*Decl*/}\n""";
      val (l, r) = rewrites.getOrElse(ctx.getStart(), ("", ""));
      rewrites.put(ctx.getStart(), (l + execOnlyOnce, r));
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

  // Every `assgExpr` is part of some `expressionStatement`,
  // which we need to access for location of token for `;`.
  private[Instrumentor] def getStatementContext(assgCtx : CParser.AssignmentExpressionContext) : CParser.ExpressionStatementContext = {
    def find(ctx : ParserRuleContext) : CParser.ExpressionStatementContext =
      ctx.getParent() match {
        case es : CParser.ExpressionStatementContext => es;
        case null => throw new IllegalStateException();
        case p => find(p);
      }

    find(assgCtx);
  }

  private[Instrumentor] def addStringConstructionFor(ctx : CParser.AssgExprContext) {
    val unaryStr = ctx.unaryExpression().getText();

    // Generate code to construct string.
    val output = try {
      val assgCType = typeInfer.visit(ctx.unaryExpression());

      if (assgCType != null) {
        generateStringConstruction(assgCType, s"${assgCType.getId} = ");
      } else {
        s"/* Couldn't find CType for $unaryStr */";
      }
    } catch {
      case e : Throwable => s"/* Couldn't find CType for $unaryStr */";
    }


    val (lb, rb) = rewrites.getOrElse(ctx.getStart(), ("", ""));
    rewrites.put(ctx.getStart(), (lb + "{/*StrCons*/ ", rb));

    // This needs to come after the semicolon of the statement.
    val stmtCtx = getStatementContext(ctx);
    val stopTok = stmtCtx.getStop();
    val (la, ra) = rewrites.getOrElse(stopTok, ("", ""));
    rewrites.put(stopTok, (la, output + " /*StrCons*/}" + ra));
  }

  private[Instrumentor] def addStringConstructionFor(ctx : CParser.ExpressionStatementContext, assgCtx : CParser.AssignmentExpressionContext) {
    assgCtx match {
      case assgExprCtx : CParser.AssgExprContext =>
        addStringConstructionFor(assgExprCtx);
      case assgFallCtx : CParser.AssgFallthroughContext => {
        val stmtCtx = getStatementContext(assgCtx);
        val stopTok = stmtCtx.getStop();

        try {
          val exprType = typeInfer.visit(assgFallCtx);

          exprType match {
            case null => ();
            case ft : FunctionType => ();
            case at : ArrayType => {
              // This special case is needed because the above does "T tmp = E;",
              // but when E is an array (identifier), this doesn't work.
              // -- It's still possible, therefore, that the worksheet will evaluate expression
              // statements with side effects (e.g. `*(ptrToArr++)`),
              // but this is "less wrong" until we solve this.

              // Array-of-char is also a special case; since a nul-terminated
              // string will output `nul`, which results in `[a, b,`
              // rather than `[a, b]` or `ab`.
              val output = generateStringConstruction(at.of match {
                case PrimitiveType(_, "char") => PrimitiveType(at.id, "char *");
                case _ => exprType;
              });

              // After the semicolon of the statement
              val (l, r) = rewrites.getOrElse(stopTok, ("", ""));
              rewrites.put(stopTok, (l, output + r));
            }
            case _ => {
              // To ensure no side-effect expressions,
              // want to transform like:
              //   E
              // becomes
              // { T res =
              //   E;
              //   output(res);
              // }

              // BUG: decln becomes "struct S;",
              // when it should be "struct S wsExprResult;"
              val resId = "wsExprResult";
              val decln = declarationOf(exprType, resId);
              val output = generateStringConstruction(exprType.changeId(resId));

              val startTok = ctx.getStart();
              val (lb, rb) = rewrites.getOrElse(startTok, ("", ""));
              rewrites.put(startTok, (lb + s"{/*StrConsE*/ $decln; $resId =", rb));

              // After the semicolon of the statement
              val (la, ra) = rewrites.getOrElse(stopTok, ("", ""));
              rewrites.put(stopTok, (la, output + " /*StrConsE*/}" + ra));
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

  private[Instrumentor] def wrapStatementWithBraces(stmt : CParser.StatementContext) {
    // We also should add a LineDirective here.
    addLineDirectiveTo(stmt);

    val iterationVarName = blockIterationIdentifierFor(stmt) + "_oneline";

    val wsDirective = WorksheetDirective(nonce);
    val printCode = wsDirective.code("[max iterations exceeded]", kind = "termination")
    val infLoopGuard = s"""static int $iterationVarName = -1;
  $iterationVarName += 1;
  if ($iterationVarName > WORKSHEET_MAX_ITERATIONS) {
    $printCode;
    exit(EXIT_SUCCESS);
  }
""";

    val startTok = stmt.getStart();
    val (lb, rb) = rewrites.getOrElse(startTok, ("", ""));
    rewrites.put(startTok, ("{/*OneLineWrap*/ \n" + infLoopGuard + lb, rb));

    val stopTok = stmt.getStop();
    val (la, ra) = rewrites.getOrElse(stopTok, ("", ""));
    rewrites.put(stopTok, (la, ra + " /*OneLineWrap*/}\n"));
  }

  override def exitSelectionStatement(ctx : CParser.SelectionStatementContext) {
    for (stmt <- ctx.statement()) {
      if (stmt.compoundStatement() == null) {
        // If the `statement` of the iterationStatement isn't a compoundStatment,
        // wrap it with { }.
        wrapStatementWithBraces(stmt);
      }
    }
  }

  override def exitIterationStatement(ctx : CParser.IterationStatementContext) {
    val stmt = ctx.statement();
    if (stmt.compoundStatement() == null) {
      // If the `statement` of the iterationStatement isn't a compoundStatment,
      // wrap it with { }.
      wrapStatementWithBraces(stmt);
    }
  }

  override def enterFunctionDefinition(ctx : CParser.FunctionDefinitionContext) {
    val compoundStmt = ctx.compoundStatement();

    // Insert after {: print "ENTER FUNCTION"
    val startTok = compoundStmt.getStart();
    val enterCode = FunctionEnterDirective(nonce).code();
    val (lb, rb) = rewrites.getOrElse(startTok, ("", ""));
    rewrites.put(startTok, (lb, rb + s"\n$enterCode\n"));

    // Insert before }: print "RETURN FUNCTION"
    val stopTok = compoundStmt.getStop();
    val returnCode = FunctionReturnDirective(nonce).code();
    val (le, re) = rewrites.getOrElse(stopTok, ("", ""));
    rewrites.put(stopTok, (le + s"\n$returnCode\n", re));
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

        val (lb, rb) = rewrites.getOrElse(ctx.getStart(), ("", ""));
        rewrites.put(ctx.getStart(), (lb + "{/*Jmp*/ " + code, rb));

        val (la, ra) = rewrites.getOrElse(ctx.getStop(), ("", ""));
        rewrites.put(ctx.getStop(), (la, "/*Jmp*/}" + ra));
      }
      // There are other jump statements, to be ignored.
      case _ => ();
    }
  }

  private[Instrumentor] def checkForFilterBlockCommentInBlock(ctx : CParser.CompoundStatementContext) {
    // We're looking for the FIRST comment after a block,
    // to see if it contains a message to 'filter' to an iteration.

    var idx = ctx.getStart.getTokenIndex() + 1;
    def tokenAt(i : Int) = tokens.get(i);

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

    val wsDirective = WorksheetDirective(nonce);
    val printCode = wsDirective.code("[max iterations exceeded]", kind = "termination")

    val (lb, rb) = rewrites.getOrElse(startTok, ("", ""));
    rewrites.put(startTok, (s"""{ /*CTR*/ static int $iterationVarName = -1;
  $iterationVarName += 1;
  if ($iterationVarName > WORKSHEET_MAX_ITERATIONS) {
    $printCode;
    exit(EXIT_SUCCESS);
  }
""" + lb, rb));


    val stopTok = ctx.getStop();
    val (le, re) = rewrites.getOrElse(stopTok, ("", ""));
    rewrites.put(stopTok, (le, " /*CTR*/ }" + re));

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
    tooler.getInstrumentedProgram();
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
