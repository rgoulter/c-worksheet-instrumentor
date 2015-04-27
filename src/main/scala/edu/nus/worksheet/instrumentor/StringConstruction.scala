package edu.nus.worksheet.instrumentor

import org.antlr.v4.runtime._
import org.antlr.v4.runtime.tree._
import scala.collection.JavaConversions._
import scala.collection.immutable.List
import scala.collection.mutable.Stack;
import scala.collection.mutable.Map;
import edu.nus.worksheet.instrumentor.Util.currentScopeForContext;
import edu.nus.worksheet.instrumentor.Util.getANTLRLexerTokensParserFor;

class StringConstruction(scopes : ParseTreeProperty[Scope]) extends CBaseListener {
  private[StringConstruction] val ctypeFromDecl = new CTypeFromDeclaration(scopes);
  private[StringConstruction] var globalScope : GlobalScope = null;

  private[StringConstruction] def allCTypes : Seq[CType] =
    if (globalScope != null) {
      globalScope.symbols.values.toSeq;
    } else {
      Seq();
    }

  // Grammar entry points are either typeInferenceFixture or compilationUnit,
  // set global scope either way.
  override def enterCompilationUnit(ctx : CParser.CompilationUnitContext) {
    currentScopeForContext(ctx, scopes) match {
      case gs : GlobalScope => globalScope = gs;
      case _ => ();
    }
  }

  override def enterTypeInferenceFixture(ctx : CParser.TypeInferenceFixtureContext) {
    currentScopeForContext(ctx, scopes) match {
      case gs : GlobalScope => globalScope = gs;
      case _ => ();
    }
  }

  override def exitEnumSpecifier(ctx : CParser.EnumSpecifierContext) {
    // Keep track of declared enums.
    if (ctx.enumeratorList() != null) {
      val enumTag = if(ctx.Identifier() != null) ctx.Identifier().getText() else null;

      if (enumTag != null) {
        val enum = ctypeFromDecl.ctypeOfEnumSpecifier(ctx);
        assert(enum.enumTag != null);
        currentScopeForContext(ctx, scopes).defineEnum(enum);
      }
    }
  }

  override def enterStructOrUnionSpecifier(ctx : CParser.StructOrUnionSpecifierContext) {
    if (ctx.structDeclarationList() != null) {
      val structTag = if (ctx.Identifier() != null) ctx.Identifier().getText() else null;

      if (structTag != null) {
        // Because a forward-declaration doesn't return the type of a
        // declared struct, we need to use a match here.
        ctypeFromDecl.ctypeOfStructOrUnionSpecifier(ctx) match {
          case struct : StructType => {
            assert(struct.structTag != null);
            currentScopeForContext(ctx, scopes).defineStruct(struct);
          }
        }
      }
    }
  }


  override def exitInitDeclarator(ctx : CParser.InitDeclaratorContext) {
    def getAncestorDecln(ctx : ParserRuleContext) : Option[CParser.DeclarationContext] = {
      assert(ctx.isInstanceOf[CParser.InitDeclaratorListContext]);
      ctx.getParent() match {
        case initDeclLCtx : CParser.InitDeclaratorListContext =>
          getAncestorDecln(initDeclLCtx);
        case ctx : CParser.MaybeInitDeclaratorListContext =>
          ctx.getParent() match {
            case decln : CParser.DeclarationContext =>
              Some(decln);
            case _ =>
              None;
          }
        case _ =>
          None;
      }
    }

    val ancestorDecln = getAncestorDecln(ctx.getParent());
    val specrs = ancestorDecln match {
      case Some(declCtx) =>
        declCtx.declarationSpecifiers();
      case None =>
        throw new IllegalStateException("Needs to have declaration as ancestor to get type");
    }

    val declnCType = ctypeFromDecl.ctypeOf(specrs, ctx);

    val currentScope = currentScopeForContext(ctx, scopes);
    if (declnCType.id != null)
      currentScope.defineSymbol(declnCType);
  }

  override def exitFunctionDefinition(ctx : CParser.FunctionDefinitionContext) {
    val specifiedType = ctypeFromDecl.ctypeOf(ctx.declarationSpecifiers());
    val definedFun = ctypeFromDecl.ctypeOfDeclarator(specifiedType, ctx.declarator())

    // Functions have their own Function scope, so we define the function
    // itself in the parent scope. (i.e. the global scope).
    val currentScope = currentScopeForContext(ctx.getParent(), scopes);
    currentScope.defineSymbol(definedFun);
  }
}

object StringConstruction {
  def fixCType(ct : CType, cid : String) : CType = {
    // As we exit initDeclarator, we need to fix the array
    // identifiers and indices.

    // 'nested arrays' may not be directly adjactent.
    // e.g. array-of-struct-with-array; array-of-ptr-to-array.
    // If want to capture dimension, use a stack.
    var arrNum = 0;
    def fixArrayIndices(arr : ArrayType, id : String) : ArrayType = {
      val arrIdx = s"${cid}_${arrNum}"; // We need the *base* index here if we want to be unique.
      arrNum += 1;
      val nextId = s"$id[$arrIdx]"; // i, j, k, ... may be more readable.

      arr.of match {
        // Array-of-array, we return an array with the next level fixed
        case nextArr @ ArrayType(_, _, m, nextOf) => ArrayType(id,
                                                               arrIdx,
                                                               arr.n,
                                                               fixArrayIndices(nextArr,
                                                                               nextId));
        // Array-of- primitive/pointer/struct. no need to adjust much.
        case c  : CType => ArrayType(id, arrIdx, arr.n, fix(c, nextId));
        case _ => throw new UnsupportedOperationException();
      }
    }

    def fixStruct(st : StructType, id : String) : StructType = {
      // Struct's members have already been "fixed"; so we only need to prefix *this* id
      // before every member (and descendant member).

      // We don't support ptr-to-struct at the moment.
      val newStructId = id + (if (st.id != null) s".${st.id}" else "");

      // Relabelling op; can we have this more consistent w/ "fixCType"?
      def prefix(ct : CType) : CType = ct match {
          case StructType(_, sOrU, tag, members) =>
            StructType(s"$newStructId.${ct.id}", sOrU, tag, members.map { mm =>
              prefix(mm);
            });
          case PrimitiveType(i, t) => PrimitiveType(s"$newStructId.$i", t);
          case PointerType(i, of) => PointerType(s"$newStructId.$i", prefix(of));
          case ArrayType(i, idx, n, of) => ArrayType(s"$newStructId.$i", idx, n, prefix(of));
          case ForwardDeclarationType(i, t) => ForwardDeclarationType(s"$newStructId.$i", t);
          // We can get null e.g. for pointers-of-pointers, or pointer-to-null.
          case null => null;
          case _ => throw new UnsupportedOperationException(s"Cannot fix struct for: $ct");
      }

      StructType(newStructId, st.structOrUnion, st.structTag, st.members.map(prefix _))
    }

    def fixPointer(p : PointerType, id : String) : PointerType = p match {
      // Need to dereference `of`.
      case PointerType(_, of) => PointerType(id, fix(of, s"(*$id)"));
    }

    def fix(c : CType, id : String) : CType = c match {
      case arr : ArrayType => fixArrayIndices(arr, id);
      case st : StructType => fixStruct(st, id);
      case ptr : PointerType => fixPointer(ptr, id);
      case EnumType(_, t, constants) => EnumType(id, t, constants);
      case PrimitiveType(_, t) => PrimitiveType(id, t);
      case FunctionType(f, r, p) => FunctionType(id, r, p);
      case ForwardDeclarationType(_, t) => ForwardDeclarationType(id, t);
      case t => t; // If it's not one of the above, we don't need to 'fix' it.
    }

    return fix(ct, cid);
  }

  def getCTypesOf(program : String) : Seq[CType] = {
    val (lexer, tokens, parser) = getANTLRLexerTokensParserFor(program);

    val tree = parser.compilationUnit();

    val walker = new ParseTreeWalker();

    val defineScopesPhase = new DefineScopesPhase();
    walker.walk(defineScopesPhase, tree);
    val scopes = defineScopesPhase.scopes;

    val strCons = new StringConstruction(scopes);
    walker.walk(strCons, tree);

    // Need to clean up any forward declarations.
    defineScopesPhase.allScopes.foreach(_.flattenForwardDeclarations());


    return strCons.allCTypes;
  }

  def getTypedefNamesOf(program : String) : Iterable[String] = {
    val (lexer, tokens, parser) = getANTLRLexerTokensParserFor(program);

    val tree = parser.compilationUnit();

    val walker = new ParseTreeWalker();

    val defineScopesPhase = new DefineScopesPhase();
    walker.walk(defineScopesPhase, tree);
    val scopes = defineScopesPhase.scopes;

    val strCons = new StringConstruction(scopes);
    walker.walk(strCons, tree);

    // Need to clean up any forward declarations.
    defineScopesPhase.allScopes.foreach(_.flattenForwardDeclarations());


    return strCons.globalScope.declaredTypedefs.keys;
  }

  def getCTypeOf(program : String) : CType = {
    val ctypes = getCTypesOf(program);
    return ctypes.get(ctypes.length - 1);
  }

  def getWithPreprocessedHeader[A](header : String, f : String => A) : Option[A] = {
    val input = s"#include <$header>";
    val prog = new CProgram(input);

    return prog.preprocessed() match {
      case Some(processed) => try {
        Some(f(processed));
      } catch {
        // Some error occured while processing the header file.
        // e.g. some feature our tools don't understand.
        case _ : Throwable => {
          None;
        }
      }
      case None => None;
    }
  }

  def getCTypesOfHeader(header : String) : Seq[CType] = {
    getWithPreprocessedHeader(header, getCTypesOf) match {
      case Some(ctypes) => ctypes;
      case None => Seq();
    }
  }

  def getTypedefNamesOfHeader(header : String) : Iterable[String] = {
    getWithPreprocessedHeader(header, getTypedefNamesOf) match {
      case Some(names) => names;
      case None => Seq();
    }
  }

  // e.g. 'stdio.h' of "#include <stdio.h>".
  // Temporary implementation, in lieu of lack of CPP support.
  //
  // Since C allows things like "#include MACRO_HEADER",
  // this will only work for some/most cases.
  def getIncludeHeadersOf(program : String) : Seq[String] = {
    val input = new ANTLRInputStream(program);
    val lexer = new CLexer(input);
    val tokens = new CommonTokenStream(lexer);
    tokens.fill();

    val ppDirectiveTokens = tokens.getTokens().filter({ tkn => tkn.getType() == CLexer.PreprocessorDirective });

    def headerFromIncludeDirective(inc : String) : Option[String] = {
      // Matches e.g.
      // #include <stdio.h>
      // # include <stdio.h>
      // # include "stdio.h"
      val IncludeRegex = """#\s*include\s*[<"](.*)[>"]\s*""".r;

      inc match {
        case IncludeRegex(h) => Some(h);
        case _ => None;
      }
    }

    return ppDirectiveTokens.map(_.getText().trim())
                            .map(headerFromIncludeDirective _)
                            .flatten;
  }

  def main(args : Array[String]) : Unit = {
    val cts = getCTypesOfHeader("stdio.h");

    for (ct <- cts) {
      println(ct.id);
    }
  }
}