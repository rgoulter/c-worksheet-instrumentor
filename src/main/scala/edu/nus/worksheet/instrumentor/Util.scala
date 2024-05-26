package edu.nus.worksheet.instrumentor

import scala.jdk.CollectionConverters.*
import org.antlr.v4.runtime.RuleContext
import org.antlr.v4.runtime.tree.ParseTreeProperty
import org.antlr.v4.runtime.CommonTokenStream
import org.antlr.v4.runtime.ANTLRInputStream
import org.antlr.v4.runtime.tree.ParseTreeWalker
import org.antlr.v4.runtime.BaseErrorListener
import org.antlr.v4.runtime.Recognizer
import org.antlr.v4.runtime.RecognitionException

private[instrumentor] object Util {
  def getANTLRLexerTokensParserFor(
      inputProgram: String
  ): (CLexer, CommonTokenStream, CParser) = {
    val headers = StringConstruction.getIncludeHeadersOf(inputProgram);
    val allTypedefNamesInHeaders =
      headers.map(HeaderUtils.getTypedefNamesOfHeader).flatten;

    val input = new ANTLRInputStream(inputProgram);
    val lexer = new CLexer(input);
    val tokens = new CommonTokenStream(lexer);
    val parser = new CParser(tokens);

    parser.addErrorListener(new BaseErrorListener {
      override def syntaxError(
          recogniser: Recognizer[?, ?],
          offendingSymbol: Any,
          line: Int,
          charPosInLine: Int,
          msg: String,
          e: RecognitionException
      ): Unit = {
        throw new ParseException(inputProgram, (line, charPosInLine), msg);
      }
    });

    parser.typedefs.addAll(allTypedefNamesInHeaders.asJava);

    (lexer, tokens, parser);
  }

  def idOfDeclarator(ctx: CParser.DirectDeclaratorContext): String =
    ctx match {
      case id: CParser.DeclaredIdentifierContext => id.getText();
      case paren: CParser.DeclaredParenthesesContext =>
        idOfDeclarator(paren.declarator().directDeclarator());
      case arr: CParser.DeclaredArrayContext =>
        idOfDeclarator(arr.directDeclarator());
      case funProto: CParser.DeclaredFunctionPrototypeContext =>
        idOfDeclarator(funProto.directDeclarator());
      case funDefn: CParser.DeclaredFunctionDefinitionContext =>
        idOfDeclarator(funDefn.directDeclarator());
    }

  def currentScopeForContext[T](
      ctx: RuleContext,
      scopes: ParseTreeProperty[Scope]
  ): Scope = {
    val scope = scopes.get(ctx);
    val parent = ctx.getParent();
    if scope != null then {
      return scope;
    } else if parent != null then {
      return currentScopeForContext[T](parent, scopes);
    } else {
      throw new IllegalStateException(
        "Assumed to have a Scope by time reaches root node."
      );
    }
  }

  def lookup(
      scopes: ParseTreeProperty[Scope],
      ctx: RuleContext,
      identifier: String
  ): Option[CType] = {
    val currentScope = currentScopeForContext(ctx, scopes);
    currentScope.resolveSymbol(identifier);
  }

  def someOrNone(s: String): Option[String] =
    if s != null then Some(s) else None;

  // For finding a common real type between two arithmetic types.
  // e.g. commonRealType("int", "float") = "float"
  //
  // For convenience, it is assumed only "type specifier" types are given.
  def commonRealType(t1: String, t2: String): String = {
    val typeSpecs =
      Seq("char", "short", "int", "long", "float", "double", "long double");
    return typeSpecs(Seq(t1, t2).map(typeSpecs.indexOf).max);
  }

  def isIntType(ct: CType): Boolean =
    ct match {
      case PrimitiveType(_, t) =>
        Seq("char", "short", "int", "long").contains(t);
      case _ => false;
    }

  def isArithmeticType(ct: CType): Boolean =
    ct match {
      case PrimitiveType(_, t) =>
        Seq("char", "short", "int", "long", "float", "double", "long double")
          .contains(t);
      case _ => false;
    }
}
