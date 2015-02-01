package edu.nus.worksheet.instrumentor;

import org.antlr.v4.runtime._
import org.antlr.v4.runtime.tree._
import scala.collection.JavaConversions._
import scala.collection.mutable.Stack;

class VarNamePhase(val tokens : BufferedTokenStream) extends CBaseListener {
  val rewriter = new TokenStreamRewriter(tokens);
  var currentType : String = "";
  var currentName : String = "";
  
  val nameTypeStack = new Stack[(String, String)]();

  private[VarNamePhase] def saveCurrentNameType() = {
    nameTypeStack.push((currentName, currentType));
  }

  private[VarNamePhase] def restoreCurrentNameType() = {
    val oldVals = nameTypeStack.pop();
    val (oldName, oldType) = oldVals;
    currentName = oldName;
    currentType = oldType;
  }

  // Declared variables may be like:
  //   int x, y[4], z[n][n];
  // i.e. declarationSpecifiers for multiple declarators
  override def enterDeclarationSpecifiers(ctx : CParser.DeclarationSpecifiersContext) = {
    // Push old type/name values onto stack
    saveCurrentNameType();
    
    // Set current type. (Preserve spaces in type).
    val declSpecrs = rewriter.getText(ctx.getSourceInterval());
    currentType = declSpecrs;
  }
  
  // The "type" of a declared variable is essentially
  //   declarationSpecifiers + non-identifier-declarators
  // so we need to delete the identifier token.
  override def enterDeclaredIdentifier(ctx : CParser.DeclaredIdentifierContext) = {
    rewriter.delete(ctx.getStart());
    
    val name = ctx.getText();
    currentName = name;
  }
  
  override def exitDeclarator(ctx : CParser.DeclaratorContext) = {
    val declaratorType = rewriter.getText(ctx.getSourceInterval());
    
    // Save/output the variable NAME + TYPE
    val fullType = Array(currentType, declaratorType).filter(!_.isEmpty()).mkString(" ");
    println("NAME: " + currentName + ", TYPE: \'" + fullType + "\'");
  }
  
  // Case 1: Typical Declaration
  override def exitDeclaration(ctx : CParser.DeclarationContext) = {
    restoreCurrentNameType();
  }
  
  // Case 2: Function (Prototype) Parameter
  override def exitParameterDeclaration(ctx : CParser.ParameterDeclarationContext) = {
    restoreCurrentNameType();
  }
}

object VarDecl {
  def main(args : Array[String]) : Unit = {
    val input = new ANTLRInputStream(System.in);
    val program = """int x = 3, y[4], z[n][n];""";
    val program2 = """int myFunc(int param1, int param2[5], float y);""";
//    val input = new ANTLRInputStream(program2);
    val lexer = new CLexer(input);
    val tokens = new CommonTokenStream(lexer);
    val parser = new CParser(tokens);

    val tree = parser.compilationUnit(); // entry rule for parser

    val walker = new ParseTreeWalker();
    val tooler = new VarNamePhase(tokens);
    walker.walk(tooler, tree);
  }
}
