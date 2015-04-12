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
    val n = if (ctx.assignmentExpression() != null) {
      rewriter.getText(ctx.assignmentExpression().getSourceInterval());
    } else {
      // declared array might not have size; e.g. arguments for functions.
      // e.g. *args[].
      null;
    }

    // we fix the array id/idx when we exit initDeclarator.
    currentType = ArrayType(null, null, n, currentType);
  }

  override def enterAbstractDeclaredArray(ctx : CParser.AbstractDeclaredArrayContext) {
    val n = if (ctx.assignmentExpression() != null) {
      rewriter.getText(ctx.assignmentExpression().getSourceInterval());
    } else {
      // declared array might not have size; e.g. arguments for functions.
      // e.g. *args[].
      null;
    }

    // abstract, so no id/idx to give to the array later.
    currentType = ArrayType(null, null, n, currentType);
  }

  override def enterParameterTypeList(ctx : CParser.ParameterTypeListContext) {
    // "function prototype" as opposed to old-style function definition.
    // i.e. it may be like "int f(int, int)", or "int f(int x)",
    //      but not like "int f()" or "int f(x)".

    saveCurrentNameType();

    // push a place-holder onto the stack.
    nameTypeStack.push(("dummy", Placeholder()));
  }

  override def exitParameterTypeList(ctx : CParser.ParameterTypeListContext) {
    // "function prototype" as opposed to old-style function definition.
    // i.e. it may be like "int f(int, int)", or "int f(int x)",
    //      but not like "int f()" or "int f(x)".

    // Pop until we get back to dummy Placeholder.
    var parameters = Seq[CType]();

    // Populate members with the stacked types.
    var reachedPlaceholder = false;
    do {
      val (id, t) = nameTypeStack.pop();

      t match {
        case Placeholder() => reachedPlaceholder = true;
        case ct : CType => parameters = t +: parameters;
      }
    } while (!reachedPlaceholder);

    restoreCurrentNameType();

    val isPtrType = currentType.isInstanceOf[PointerType];
    val isFunType = currentType.isInstanceOf[FunctionType];
    println(s"isPtr? $isPtrType, isFun? $isFunType, isDeclrPtr? $isDeclaratorPointer");

    // PointerType assumes that the currentType is set by the time it enters
    // the pointer AST node. So, for function-pointer declarations, the current state
    // ... is complicated.
    currentType match {
      case PointerType(pid, pof) => {
        if (isDeclaratorPointer) {
          val returnType = currentType;
          val funcName = currentId;
          currentType = FunctionType(currentId, returnType, parameters);
        } else {
          currentType = PointerType(null, null);
        }
      }
      case FunctionType(fid, fr, fargs) => {
        currentType = FunctionType(fid, PointerType(null, null), fargs);
      }
      case ct => {
        val returnType = currentType;
        val funcName = currentId;
        currentType = FunctionType(currentId, returnType, parameters);
      }
    };
  }

  override def enterPointer(ctx : CParser.PointerContext) {
    // Discard the currentType.
    currentType = currentType match {
      case PrimitiveType(id, "char") => PrimitiveType(id, "char *"); // assume nul-terminated string.
      case PrimitiveType(id, "void") => PointerType(null, null); // cannot output void-ptr.
      case ptr : PointerType => PointerType(null, null); // Discard 'of' for ptr-to-ptr.

      // We want function pointers for type inference e.g "(*fp)(x)"
      // but also need to ensure ST4 handles printing "functions" sensibly.
      // For now, just discard function pointers.
      case f : FunctionType => PointerType(null, null); // Discard function pointers.
      case t => PointerType(null, t);
    }
  }
  
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
          case StructType(_, tag, members) =>
            StructType(s"$newStructId.${ct.id}", tag, members.map { mm =>
              prefix(mm);
            });
          case PrimitiveType(i, t) => PrimitiveType(s"$newStructId.$i", t);
          case PointerType(i, of) => PointerType(s"$newStructId.$i", prefix(of));
          case ArrayType(i, idx, n, of) => ArrayType(s"$newStructId.$i", idx, n, prefix(of));
          case _ => throw new UnsupportedOperationException();
      }

      StructType(newStructId, st.structType, st.members.map(prefix _))
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
      case t => t; // If it's not one of the above, we don't need to 'fix' it.
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

  override def exitInitializer(ctx : CParser.InitializerContext) {
    // for initDeclarators like "x[] = { ... }",
    // the top-level dimension isn't given, and must be inferred from initializer.
    if (ctx.initializerList() != null) {
      def initializerLength(dlCtx : CParser.InitializerListContext) : Int = {
        val nextCtx = dlCtx.initializerList();
        if (nextCtx != null)
          1 + initializerLength(nextCtx);
        else
          1;
      }

      val n = initializerLength(ctx.initializerList());

      currentType = currentType match {
        case ArrayType(id, idx, _, of) => ArrayType(id, idx, n.toString(), of);
        case _ => currentType; // Can ignore.
      }
    }
  }

  override def exitInitDeclarator(ctx : CParser.InitDeclaratorContext) {
    if (isInDeclarationContextWithTypedef(ctx)) {
      declaredTypedefs += currentId -> currentType;
    } else {
      // Need to keep a copy of this around *without* member types
      // being given the currentId.
      // e.g. struct S s1 = { x = 2 }, s2;
      // Need to have `s2.x` as well as `s1.x`.
      val unfixedType = currentType;

      currentType = fixCType(currentType, currentId);
    
      // Add the type to the list of all CTypes,
      // and define it in the current scope.
      allCTypes = allCTypes :+ currentType;
      val currentScope = currentScopeForContext(ctx, scopes);
      currentScope.define(currentId, currentType);

      currentType = unfixedType;
    }
  }

  override def exitFunctionDefinition(ctx : CParser.FunctionDefinitionContext) {
    currentType = fixCType(currentType, currentId);

    // Add the type to the list of all CTypes,
    // and define it in the current scope.
    allCTypes = allCTypes :+ currentType;
    val currentScope = currentScopeForContext(ctx, scopes);
    currentScope.define(currentId, currentType);
  }

  override def enterParameterDeclaration(ctx : CParser.ParameterDeclarationContext) {
    // Correct the currentId when the declaration is abstract
    if (ctx.declarator() == null) {
      currentId = null;
    }
  }

  // Collect the parameterDeclaration members by pushing the id,type onto name,type stack
  override def exitParameterDeclaration(ctx : CParser.ParameterDeclarationContext) {
    if (ctx.declarator() != null) {
      // Only "fix" the current type if it's not abstractDeclarator
      currentType = fixCType(currentType, currentId);
    }

    // Push the name/type onto stack, so exitStructDeclarationList can add it to members
    saveCurrentNameType();
  }

  // Collect the structDeclarator members by pushing the id,type onto name,type stack
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

  override def exitEnumSpecifier(ctx : CParser.EnumSpecifierContext) {
    if (ctx.enumeratorList() != null) {
      // Unlike struct, let's just manually traverse tree to get constants.
      var constants = Seq[String](); 

      var list = ctx.enumeratorList();
      while (list != null) {
        constants = list.enumerator().enumerationConstant().getText() +: constants;
        list = list.enumeratorList();
      }

      val enumTag = if(ctx.Identifier() != null) ctx.Identifier().getText() else null;
      val enum = EnumType(null, enumTag, constants);
      currentType = enum;

      if (enumTag != null) {
        declaredEnums += enumTag -> enum;
      }
    } else {
      val enumTag = ctx.Identifier().getText();
      declaredEnums.get(enumTag) match {
        case Some(enum) => currentType = enum;
        case None => throw new RuntimeException(s"struct $enumTag undeclared!");
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