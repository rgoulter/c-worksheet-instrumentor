package edu.nus.worksheet.instrumentor

import scala.collection.mutable;
import org.antlr.v4.runtime._
import org.antlr.v4.runtime.tree._
import edu.nus.worksheet.instrumentor.Util.idOfDeclarator;

class DefineScopesPhase extends CBaseListener {
  val scopes = new ParseTreeProperty[Scope]();
  var globals : GlobalScope = _;
  var currentScope : Scope = _;

  private[instrumentor] val allScopes = mutable.ArrayBuffer[Scope]();

  private[DefineScopesPhase] val blockNums = new mutable.Stack[Int];
  private[DefineScopesPhase] var blockNum : Int = 0;

  def saveScope(ctx : ParserRuleContext, s : Scope) = {
    scopes.put(ctx, s);
    allScopes += s;
  }

  // For entry-level rules.
  private[DefineScopesPhase] def setupGlobalScope(ctx : ParserRuleContext) {
    globals = new GlobalScope();

    // Save global scope to CompilationUnit ctx,
    // so that global scope is accessible from ParseTreeProperty.
    saveScope(ctx, globals);
    currentScope = globals;
  }

  override def enterCompilationUnit(ctx : CParser.CompilationUnitContext) =
    setupGlobalScope(ctx);

  override def enterTypeInferenceFixture(ctx : CParser.TypeInferenceFixtureContext) =
    setupGlobalScope(ctx);

  override def enterFunctionDefinition(ctx : CParser.FunctionDefinitionContext) = {
    // Reset the block counter; needs to start from 0 for a function.
    blockNum = 0;

    val name = idOfDeclarator(ctx.declarator().directDeclarator());

    assert(currentScope.isInstanceOf[GlobalScope]);

    // Push Scope: fn scope points to enclosing scope. (GlobalScope in C).
    val functionScope = new FunctionScope(Some(currentScope), name);

    saveScope(ctx, functionScope);
    currentScope = functionScope;
  }

  override def exitFunctionDefinition(ctx : CParser.FunctionDefinitionContext) =
    currentScope = currentScope.enclosingScope match {
      case Some(scope) => scope;
      case None => throw new IllegalStateException("Function doesn't have enclosing scope");
    }

  override def enterCompoundStatement(ctx : CParser.CompoundStatementContext) = {
    val blockName = s"${currentScope.scopeName}_$blockNum";
    blockNums.push(blockNum);
    blockNum = 0;

    currentScope = new BlockScope(Some(currentScope), blockName);
    saveScope(ctx, currentScope);
  }

  override def exitCompoundStatement(ctx : CParser.CompoundStatementContext) = {
    currentScope = currentScope.enclosingScope match {
      case Some(scope) => scope;
      case None => throw new IllegalStateException("Compound Statement doesn't have enclosing scope");
    }

    blockNum = blockNums.pop();
    blockNum += 1;
  }
}