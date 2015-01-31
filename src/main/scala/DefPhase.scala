import org.antlr.v4.runtime.{ParserRuleContext, Token};
import org.antlr.v4.runtime.tree.ParseTreeProperty;

class DefPhase extends CBaseListener {
  val scopes = new ParseTreeProperty[Scope]();
  var globals : GlobalScope = _;
  var currentScope : Scope = _;

  override def enterCompilationUnit(ctx : CParser.CompilationUnitContext) = {
    globals = new GlobalScope(None);
    currentScope = globals;
  }

  def saveScope(ctx : ParserRuleContext, s : Scope) = scopes.put(ctx, s);

  // override def exitCompilationUnit(ctx : CParser.CompilationUnitContext) = {
  //
  // }

  override def enterFunctionDefinition(ctx : CParser.FunctionDefinitionContext) = {
    val name : String = "lol"; // FIXME ctx.declarator().directDeclarator().directDeclarator().getText();

    // type of function is optional in C.
    // as we know, by default it is ... int?
    // int tokenType = 

    // Push Scope: fn scope points to enclosing scope. (GlobalScope in C).
    val function = new FunctionSymbol(name, "sometype", Some(currentScope));
    currentScope.define(function);
    saveScope(ctx, function);
    currentScope = function;
  }

  override def exitFunctionDefinition(ctx : CParser.FunctionDefinitionContext) =
    currentScope = currentScope.enclosingScope match {
      case Some(scope) => scope;
      case None => throw new IllegalStateException("Function doesn't have enclosing scope");
    }

  override def enterCompoundStatement(ctx : CParser.CompoundStatementContext) = {
    currentScope = new LocalScope(Some(currentScope));
    saveScope(ctx, currentScope);
  }

  override def exitCompoundStatement(ctx : CParser.CompoundStatementContext) =
    currentScope = currentScope.enclosingScope match {
      case Some(scope) => scope;
      case None => throw new IllegalStateException("Function doesn't have enclosing scope");
    }

  override def enterParameterDeclaration(ctx : CParser.ParameterDeclarationContext) = {
    // somehow defineVar(type, ctx.ID().getSymbol())
    // int x;
    // int someArr[5];

    // how to get type?
    // how to get name?
  }

  override def enterDeclaration(ctx : CParser.DeclarationContext) = {
    // int x;
    // int x, y, z; i.e. can be multiple
    // int someArr[5];

    // How to get *type* from this ctx?
    // How to get var name?
  }

  // We could consistently pass type/name to defineVar, if that would help.
  def defineVar(symType : String, name : String) = {
    val varSym = new VariableSymbol(name, symType);
    currentScope.define(varSym);
  }
}
