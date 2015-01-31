// CDecl Proof of Concept in Scala.
// perhaps move to its own project in Java.
import org.antlr.v4.runtime._
import org.antlr.v4.runtime.tree._
import scala.collection.JavaConversions._

// Pass through a "gibberish" (C) declaration, come up with English terms.
// Since we want a String from each result, a Visitor is appropriate;
// (we can then also control *what* we visit, so don't double count).
class GibberishPhase(val tokens : BufferedTokenStream) extends CBaseVisitor[String] {
  val rewriter = new TokenStreamRewriter(tokens);
  
  override def visitDeclaration(ctx : CParser.DeclarationContext) : String = {
    val declSpecs = visit(ctx.declarationSpecifiers());
    val theRest = visit(ctx.initDeclaratorList()); // assume only one variable for now.
    
    return theRest + declSpecs;
  }

  override def visitDeclarationSpecifiers(ctx : CParser.DeclarationSpecifiersContext) : String = {
    rewriter.getText(ctx.getSourceInterval());
  }
  
  override def visitPointer(ctx : CParser.PointerContext) : String = {
    return "pointer to " + (if (ctx.pointer() != null) visit(ctx.pointer()) else "");
  }
  
  // because "int x" fucks up, and so "int x = 3" fucks up also, need this:
  override def visitInitDeclarator(ctx : CParser.InitDeclaratorContext) =
    visit(ctx.declarator());
  
  override def visitDeclaredParentheses(ctx : CParser.DeclaredParenthesesContext) : String =
    visit(ctx.declarator());
  
  override def visitDeclarator(ctx : CParser.DeclaratorContext) : String = {
    val directDecl = visit(ctx.directDeclarator());
    return directDecl + (if (ctx.pointer() != null) visit(ctx.pointer()) else "");
  }
  
  override def visitDeclaredIdentifier(ctx : CParser.DeclaredIdentifierContext) : String = {
    rewriter.getText(ctx.getSourceInterval()) + " is ";
  }
  
  override def visitDeclaredArray(ctx : CParser.DeclaredArrayContext) : String = {
    // assignmentExpression not guaranteed; may be '*' in func. arg.
    val directDecl = visit(ctx.directDeclarator());
    val arrSizeExpr = rewriter.getText(ctx.assignmentExpression().getSourceInterval()); // visit?
    return directDecl + "array " + arrSizeExpr + " of ";
  }
  
  override def visitDeclaredFunctionPrototype(ctx : CParser.DeclaredFunctionPrototypeContext) : String = {
    val directDecl = visit(ctx.directDeclarator());
    // TODO: Look at each ... declSpecifiers2
    val params = rewriter.getText(ctx.parameterTypeList().getSourceInterval());
    
    return directDecl + "function (" + params + ") returning "
  }
}

object CDecl {
  def runForInput(input : ANTLRInputStream) = {
    val lexer = new CLexer(input);
    val tokens = new CommonTokenStream(lexer);
    val parser = new CParser(tokens);

    val tree = parser.declaration(); // entry rule for parser

    val walker = new ParseTreeWalker();
    val tooler = new GibberishPhase(tokens);
    println(tooler.visit(tree));
  }
  
  def runDeclaration(input : String) = {
    println(input);
    runForInput(new ANTLRInputStream(input) : ANTLRInputStream);
    println();
  }
  
  def main(args : Array[String]) = {
    runDeclaration("int x = 3;");
    runDeclaration("int x[4];");
    runDeclaration("int *x;");

    runDeclaration("int *x[3];");
    runDeclaration("int (*x[5])[3];");
    runDeclaration("int x[5][3];");

    runDeclaration("int *(*x)[3];");
    runDeclaration("int (**x)[3];");
    runDeclaration("int **x[3];");
    
    runDeclaration("int (*x)(void);");
  }
}