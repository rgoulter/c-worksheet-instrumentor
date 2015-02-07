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
  
  def lookup(identifier : String) : Option[CType] = {
    // By right, must consider *scope* in order to do lookup.
    allCTypes.find { ct => ct.id == identifier }
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
    currentType = new PrimitiveType(null, ctype);
  }
  
  override def exitTypeSpecifierTypedef(ctx : CParser.TypeSpecifierTypedefContext) {
    // For string construction, typedefs aren't informative to us;
    // so we look up from what's already been declared.
    val typedefId = ctx.getText();
    
    declaredTypedefs.get(typedefId) match {
      case Some(ctype) => currentType = ctype;
      case None => throw new IllegalStateException("Grammar guarantees typedef has been declared!");
    }
  }
  
  // visit-on-exit => children have been visited already.
  // so we visit-on-entry instead.
  override def enterDeclaredArray(ctx : CParser.DeclaredArrayContext) {
    val n = rewriter.getText(ctx.assignmentExpression().getSourceInterval());
    
    // we fix the array id/idx when we exit initDeclarator.
    currentType = ArrayType(null, null, n, currentType);
  }
  
  override def enterPointer(ctx : CParser.PointerContext) {
    // Discard the currentType.
    currentType = currentType match {
      case PrimitiveType(id, "char") => PrimitiveType(id, "char *"); // assume nul-terminated string.
      case _ => PointerType(null);
    }
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
      // Struct's members have already been "fixed"; so we only need to prefix *this* id
      // before every member (and descendant member).
      
      // We don't support ptr-to-struct at the moment.
      val newStructId = id + (if (st.id != null) s".${st.id}" else "");
      
      // Relabelling op; can we have this more consistent w/ "fixCType"?
      def prefix(ct : CType) : CType = ct match {
          case StructType(_, tag, members) =>
            StructType(s"$newStructId.${ct.id}", tag, members.map { mm =>
              prefix(mm);
            });
          case PrimitiveType(i, t) => PrimitiveType(s"$newStructId.$i", t);
          case PointerType(i) => PointerType(s"$newStructId.$i");
          case ArrayType(i, idx, n, of) => ArrayType(s"$newStructId.$i", idx, n, prefix(of));
          case _ => throw new UnsupportedOperationException();
      }

      StructType(newStructId, st.structType, st.members.map(prefix _))
    }
    
    def fix(c : CType, id : String) : CType = c match {
      case arr : ArrayType => fixArrayIndices(arr, id);
      case st : StructType => fixStruct(st, id);
      case PrimitiveType(_, t) => PrimitiveType(id, t);
      case PointerType(_) => PointerType(id);
    }
    
    return fix(ct, cid);
  }
  
  def isInDeclarationContextWithTypedef(ctx : CParser.InitDeclaratorContext)
  : Boolean = {
    // Check if this declaration isTypedef.
    // Find declarationContext, ancestor of initDeclaratorContext
    var declarationCtx : CParser.DeclarationContext = null;
    var parentCtx : ParserRuleContext = ctx.getParent();
    while (declarationCtx == null) {
      parentCtx match {
        case c : CParser.DeclarationContext => declarationCtx = c;
        case _ => parentCtx = parentCtx.getParent();
      }
    }
    
    return declarationCtx.isTypedef;
  }
  
  override def exitInitDeclarator(ctx : CParser.InitDeclaratorContext) {
    if (isInDeclarationContextWithTypedef(ctx)) {
      declaredTypedefs += currentId -> currentType;
    } else {
      currentType = fixCType(currentType, currentId);
    
      allCTypes = allCTypes :+ currentType;
    }

  }
  
  override def exitStructDeclarator(ctx : CParser.StructDeclaratorContext) {
    currentType = fixCType(currentType, currentId);
    
    // Push the name/type onto stack, so exitStructDeclarationList can add it to members
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

      val struct = StructType(null, structTag, members.toSeq);
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
    return ctypes.get(ctypes.length - 1);
  }
}