package edu.nus.worksheet.instrumentor

import org.antlr.v4.runtime._
import org.antlr.v4.runtime.tree._
import scala.collection.JavaConversions._
import scala.collection.immutable.List
import scala.collection.mutable.Stack;
import scala.collection.mutable.Map;
import edu.nus.worksheet.instrumentor.CParser.InitDeclaratorContext;
import edu.nus.worksheet.instrumentor.Util.currentScopeForContext;

class StringConstruction(val tokens : BufferedTokenStream, scopes : ParseTreeProperty[Scope[CType]]) extends CBaseListener {
  val rewriter = new TokenStreamRewriter(tokens);
  
  private[StringConstruction] var currentId : String = _;
  private[StringConstruction] var currentType : CType = _;

  private[StringConstruction] val nameTypeStack = new Stack[(String, CType)]();
  
  private[StringConstruction] var allCTypes = Seq[CType]();
  val declaredStructs = Map[String, StructType]();
  val declaredEnums = Map[String, EnumType]();
  val declaredTypedefs = Map[String, CType]();

  private[StringConstruction] def saveCurrentNameType() = {
    nameTypeStack.push((currentId, currentType));
  }

  private[StringConstruction] def restoreCurrentNameType() = {
    val oldVals = nameTypeStack.pop();
    val (oldName, oldType) = oldVals;
    currentId = oldName;
    currentType = oldType;
  }
  
  def lookup(ctx : RuleContext, identifier : String) : Option[CType] = {
    val currentScope = currentScopeForContext(ctx, scopes);
    return currentScope.resolve(identifier);
  }
  
  def ctypeOf(specsCtx : CParser.DeclarationSpecifiersContext, declrCtx : CParser.DeclaratorContext) : CType = {
    // Refactoring.
    // Do the minimum amount of work to get each test passing.
    val id = declrCtx.getText();
    val ptype = specsCtx.getText();
    return PrimitiveType(id, ptype);
  }

  def ctypeOf(specsCtx : CParser.DeclarationSpecifiersContext, initDeclrCtx : CParser.InitDeclaratorContext) : CType = {
    // Check initializer; may need it for Arrays
    return ctypeOf(specsCtx, initDeclrCtx.declarator());
  }

  def listOfInitDeclrList(ctx : CParser.InitDeclaratorListContext) : Seq[CParser.InitDeclaratorContext] =
    if (ctx.initDeclaratorList() != null) {
      listOfInitDeclrList(ctx.initDeclaratorList()) :+ ctx.initDeclarator();
    } else {
      Seq(ctx.initDeclarator());
    }

  override def exitDeclaration(ctx : CParser.DeclarationContext) {
    // Derive what CTypes we can from the declaration.
    val initDeclrs = listOfInitDeclrList(ctx.initDeclaratorList());

    // For each, call to ctypeOf
    val declnCTypes = initDeclrs.map(ctypeOf(ctx.declarationSpecifiers(), _));
    allCTypes = allCTypes ++ declnCTypes;
  }
}

object StringConstruction {
  def getCTypesOf(program : String) : Seq[CType] = {
    val input = new ANTLRInputStream(program);
    val lexer = new CLexer(input);
    val tokens = new CommonTokenStream(lexer);
    val parser = new CParser(tokens);

    val tree = parser.compilationUnit();

    val walker = new ParseTreeWalker();

    val defineScopesPhase = new DefineScopesPhase[CType]();
    walker.walk(defineScopesPhase, tree);
    val scopes = defineScopesPhase.scopes;

    val strCons = new StringConstruction(tokens, scopes);
    walker.walk(strCons, tree);

    return strCons.allCTypes;
  }
  
  def getCTypeOf(program : String) : CType = {
    val ctypes = getCTypesOf(program);
    return ctypes.get(ctypes.length - 1);
  }
}