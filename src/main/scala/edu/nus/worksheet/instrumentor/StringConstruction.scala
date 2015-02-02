package edu.nus.worksheet.instrumentor

import org.antlr.v4.runtime._
import org.antlr.v4.runtime.tree._
import scala.collection.JavaConversions._
import scala.collection.immutable.List
import scala.collection.mutable.Stack;
import scala.collection.mutable.Map;
import edu.nus.worksheet.instrumentor.CParser.InitDeclaratorContext

class StringConstruction(val tokens : BufferedTokenStream) extends CBaseListener {
  val rewriter = new TokenStreamRewriter(tokens);
  
  var currentId : String = _;
  var currentType : CType = _;
  
  val nameTypeStack = new Stack[(String, CType)]();
  
  var allCTypes = Seq[CType]();
  val declaredStructs = Map[String, StructType]();

  private[StringConstruction] def saveCurrentNameType() = {
    nameTypeStack.push((currentId, currentType));
  }

  private[StringConstruction] def restoreCurrentNameType() = {
    val oldVals = nameTypeStack.pop();
    val (oldName, oldType) = oldVals;
    currentId = oldName;
    currentType = oldType;
  }
  
  override def exitDeclaredIdentifier(ctx : CParser.DeclaredIdentifierContext) {
    currentId = rewriter.getText(ctx.getSourceInterval());
  }
  
  // Since we construct strings only for simple types,
  // (i.e. not for functions), we don't need to worry about "stacking" type.
  override def exitTypeSpecifierPrimitive(ctx : CParser.TypeSpecifierPrimitiveContext) {
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
  
  override def enterPointer(ctx : CParser.PointerContext) {
    // Discard the currentType.
    currentType = PointerType("??");
  }
  
  def fixCType(ct : CType, cid : String) : CType = {
    // As we exit initDeclarator, we need to fix the array
    // identifiers and indices.
    
    def fixArrayIndices(arr : ArrayType, id : String, dim : Int = 0) : ArrayType = {
      val arrIdx = s"${cid}_${dim}"; // We need the *base* index here if we want to be unique.
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
        case c  : CType => ArrayType(id, arrIdx, arr.n, fix(c, nextId));
        case _ => throw new UnsupportedOperationException();
      }
    }
    
    def fixStruct(st : StructType, id : String) : StructType = {
      // We don't support ptr-to-struct at the moment.
      StructType(id, st.structType, st.members.map { m => fix(m, s"$id.${m.id}") })
    }
    
    def fix(c : CType, id : String) : CType = c match {
      case arr : ArrayType => fixArrayIndices(arr, id);
      case st : StructType => fixStruct(st, id);
      case PrimitiveType(_, t) => PrimitiveType(id, t);
      case PointerType(_) => PointerType(id);
    }
    
    return fix(ct, cid);
  }
  
  override def exitInitDeclarator(ctx : CParser.InitDeclaratorContext) {
    currentType = fixCType(currentType, currentId);
    
    // Add to list of all found CTypes.
    allCTypes = allCTypes :+ currentType;
  }
  
  override def exitStructDeclarator(ctx : CParser.StructDeclaratorContext) {
    currentType = fixCType(currentType, currentId);
    
    // Push the name/type onto stack, so exitStructDeclarationList can add it to members
    println("exit struct declarator for " + currentId);
    saveCurrentNameType();
  }
  
  override def enterStructOrUnionSpecifier(ctx : CParser.StructOrUnionSpecifierContext) {
    saveCurrentNameType();

    // push a place-holding dummy StructType onto the stack.
    nameTypeStack.push(("dummy", Placeholder()));
  }
  
  // structOrUnionSpecifier has one of two sub-rules:
  //   struct Identifier? { structDeclList };
  //   struct Identifier
  //   
  override def exitStructOrUnionSpecifier(ctx : CParser.StructOrUnionSpecifierContext) {
    // Pop until we get back to dummy Placeholder.
    var members = Seq[CType]();
    
    // Populate members with the stacked types.
    var reachedPlaceholder = false;
    do {
      val (id, t) = nameTypeStack.pop();
      
      t match {
        case Placeholder() => reachedPlaceholder = true;
        case ct : CType => members = t +: members;
      }
    } while (!reachedPlaceholder);

    restoreCurrentNameType();

    // Create Struct

    
    if (ctx.structDeclarationList() != null) {
      // in the form of "struct Identifier? { structDeclList };",
      // (null for anonymous struct).
      val structTag = if (ctx.Identifier() != null) ctx.Identifier().getText() else null;

      val struct = StructType("??", structTag, members.toSeq);
      currentType = struct;
      
      if (structTag != null) {
        declaredStructs += structTag -> struct;
      }
    } else {
      val structTag = ctx.Identifier().getText();
      declaredStructs.get(structTag) match {
        case Some(struct) => currentType = struct;
        case None => throw new RuntimeException(s"struct $structTag undeclared!");
      }
    }
  }
}

object StringConstruction {
  def getCTypesOf(program : String) : Seq[CType] = {
    val input = new ANTLRInputStream(program);
    val lexer = new CLexer(input);
    val tokens = new CommonTokenStream(lexer);
    val parser = new CParser(tokens);

    val tree = parser.compilationUnit();

    val strCons = new StringConstruction(tokens);
    val walker = new ParseTreeWalker();
    walker.walk(strCons, tree);
    
    return strCons.allCTypes;
  }
  
  def getCTypeOf(program : String) : CType = {
    val ctypes = getCTypesOf(program);
    return ctypes.get(0); // return the first one.
  }
}