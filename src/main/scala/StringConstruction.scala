import org.antlr.v4.runtime._
import org.antlr.v4.runtime.tree._
import scala.collection.JavaConversions._
import scala.collection.mutable.Stack;

class StringConstruction(val tokens : BufferedTokenStream) extends CBaseListener {
  val rewriter = new TokenStreamRewriter(tokens);
  
  var currentId : String = _;
  var currentType : CType = _;
  
  override def exitDeclaredIdentifier(ctx : CParser.DeclaredIdentifierContext) {
    currentId = rewriter.getText(ctx.getSourceInterval());
  }
  
  // Since we construct strings only for simple types,
  // (i.e. not for functions), we don't need to worry about "stacking" type.
  override def exitTypeSpecifier(ctx : CParser.TypeSpecifierContext) {
    // Assume each declaration has only one type specifier.
    
    // We don't know the identifier at this node;
    val ctype = ctx.getText();
    currentType = new PrimitiveType("??", ctype);
  }
  
  // visit-on-exit => children have been visited already.
  // so we visit-on-entry instead.
  override def enterDeclaredArray(ctx : CParser.DeclaredArrayContext) {
    val n = rewriter.getText(ctx.assignmentExpression().getSourceInterval());
    
    // we fix the array id/idx when we exit initDeclarator.
    currentType = ArrayType("??", "??", n, currentType);
  }
  
  override def exitInitDeclarator(ctx : CParser.InitDeclaratorContext) {
    // As we exit initDeclarator, we need to fix the array
    // identifiers and indices.
    
    def fixArrayIndices(arr : ArrayType, id : String = currentId, dim : Int = 0) : ArrayType = {
      val arrIdx = s"${currentId}_${dim}"; // We need the *base* index here if we want to be unique.
      val nextId = s"$id[$arrIdx]"; // i, j, k, ... may be more readable.

      arr.of match {
        // Array-of-array, we return an array with the next level fixed
        case nextArr @ ArrayType(_, _, m, nextOf) => ArrayType(id,
                                                               arrIdx,
                                                               arr.n,
                                                               fixArrayIndices(nextArr,
                                                                               nextId,
                                                                               dim + 1));
        // Array-of- primitive/pointer/struct. no need to adjust much.
        case PrimitiveType(_, t) => ArrayType(id,
                                              arrIdx,
                                              arr.n,
                                              PrimitiveType(nextId, t));
        case _ => throw new UnsupportedOperationException();
      }
    }
    
    currentType = currentType match {
      case arr : ArrayType => fixArrayIndices(arr);
      case PrimitiveType(_, t) => PrimitiveType(currentId, t);
      case otherwise => otherwise;
    }
  }
}

object StringConstruction {
  def getCTypeOf(program : String) : CType = {
    val input = new ANTLRInputStream(program);
    val lexer = new CLexer(input);
    val tokens = new CommonTokenStream(lexer);
    val parser = new CParser(tokens);

    val tree = parser.declaration(); // entry rule for parser

    val strCons = new StringConstruction(tokens);
    val walker = new ParseTreeWalker();
    walker.walk(strCons, tree);
    
    return strCons.currentType;
  }
}