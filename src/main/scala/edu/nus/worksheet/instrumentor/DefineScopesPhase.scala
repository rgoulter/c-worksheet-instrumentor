package edu.nus.worksheet.instrumentor

import org.antlr.v4.runtime._
import org.antlr.v4.runtime.tree._
import edu.nus.worksheet.instrumentor.Util.idOfDeclarator;

class DefineScopesPhase[T] extends CBaseListener {
  val scopes = new ParseTreeProperty[Scope[T]]();
  var globals : GlobalScope[T] = _;
  var currentScope : Scope[T] = _;

  def saveScope(ctx : ParserRuleContext, s : Scope[T]) =
    scopes.put(ctx, s);

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
    val name = idOfDeclarator(ctx.declarator().directDeclarator());

    assert(currentScope.isInstanceOf[GlobalScope[T]]);

    // Push Scope: fn scope points to enclosing scope. (GlobalScope in C).
    val functionScope = new FunctionScope[T](Some(currentScope), name);

    saveScope(ctx, functionScope);
    currentScope = functionScope;
  }

  override def exitFunctionDefinition(ctx : CParser.FunctionDefinitionContext) =
    currentScope = currentScope.enclosingScope match {
      case Some(scope) => scope;
      case None => throw new IllegalStateException("Function doesn't have enclosing scope");
    }

  override def enterCompoundStatement(ctx : CParser.CompoundStatementContext) = {
    currentScope = new BlockScope(Some(currentScope));
    saveScope(ctx, currentScope);
  }

  override def exitCompoundStatement(ctx : CParser.CompoundStatementContext) =
    currentScope = currentScope.enclosingScope match {
      case Some(scope) => scope;
      case None => throw new IllegalStateException("Compound Statement doesn't have enclosing scope");
    }
}
