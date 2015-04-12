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

  def listOfInitDeclrList(ctx : CParser.InitDeclaratorListContext) : Seq[CParser.InitDeclaratorContext] =
    if (ctx.initDeclaratorList() != null) {
      listOfInitDeclrList(ctx.initDeclaratorList()) :+ ctx.initDeclarator();
    } else {
      Seq(ctx.initDeclarator());
    }

  def listOfPointer(ctx : CParser.PointerContext) : Seq[String] =
    if (ctx.pointer() != null) {
      listOfPointer(ctx.pointer()) :+ ctx.getChild(0).getText();
    } else {
      Seq(ctx.getText());
    }

  def pointerTypeOfDeclaredType(declaredType : CType, pointer : CParser.PointerContext) : CType = {
    val pointers = if (pointer != null) listOfPointer(pointer) else Seq();

    return pointers.foldLeft(declaredType)({ (ct, ptr) =>
      ct match {
        case PrimitiveType(id, "char") => PrimitiveType(id, "char *"); // assume nul-terminated string.
        case PrimitiveType(id, "void") => PointerType(null, null); // cannot output void-ptr.
        case ptr : PointerType => PointerType(null, null); // Discard 'of' for ptr-to-ptr.

        // We want function pointers for type inference e.g "(*fp)(x)"
        // but also need to ensure ST4 handles printing "functions" sensibly.
        // For now, just discard function pointers.
        case f : FunctionType => PointerType(null, null); // Discard function pointers.
        case t => PointerType(null, t);
      }
    });
  }

  def isInDeclarationContextWithTypedef(ctx : ParserRuleContext) : Boolean =
    // Check if this declaration isTypedef.
    // Find declarationContext, ancestor of ctx, if it's there.
    ctx match {
      case declarationCtx : CParser.DeclarationContext => declarationCtx.isTypedef;
      case _ => {
        if (ctx.getParent() != null)
          isInDeclarationContextWithTypedef(ctx.getParent());
        else
          false;
      }
    }

  def ctypeOfEnumSpecifier(ctx : CParser.EnumSpecifierContext) : EnumType =
    if (ctx.enumeratorList() != null) {
      var constants = Seq[String]();

      var list = ctx.enumeratorList();
      while (list != null) {
        constants = list.enumerator().enumerationConstant().getText() +: constants;
        list = list.enumeratorList();
      }

      val enumTag = if(ctx.Identifier() != null) ctx.Identifier().getText() else null;

      EnumType(null, enumTag, constants);
    } else {
      val enumTag = ctx.Identifier().getText();
      declaredEnums.get(enumTag) match {
        case Some(enum) => enum;
        case None => throw new RuntimeException(s"struct $enumTag undeclared!");
      }
    }

  override def exitEnumSpecifier(ctx : CParser.EnumSpecifierContext) {
    // Keep track of declared enums.
    if (ctx.enumeratorList() != null) {
      val enumTag = if(ctx.Identifier() != null) ctx.Identifier().getText() else null;

      if (enumTag != null) {
        val enum = ctypeOfEnumSpecifier(ctx);
        declaredEnums += enumTag -> enum;
      }
    }
  }

  def flattenStructDeclarationSpecifiers(specsCtx : CParser.SpecifierQualifierListContext) : Seq[RuleContext] = {
    def asList(ctx : CParser.SpecifierQualifierListContext) : Seq[RuleContext] =
      if (ctx.specifierQualifierList() != null) {
        ctx.getChild(0).asInstanceOf[RuleContext] +: asList(ctx.specifierQualifierList());
      } else {
        Seq(ctx.getChild(0).asInstanceOf[RuleContext]);
      }

    asList(specsCtx).map({ specOrQual =>
      specOrQual match {
        case spec : CParser.TypeSpecifierContext => Some(spec);
        // Discard type qualifiers.
        case qual : CParser.TypeQualifierContext => None;
      }
    }).flatten;
  }

  // Ignore bitfields.
  def declaratorsOfStructDeclaratorList(ctx : CParser.StructDeclaratorListContext) : Seq[CParser.DeclaratorContext] =
    if (ctx.structDeclaratorList() != null) {
      declaratorsOfStructDeclaratorList(ctx.structDeclaratorList()) :+ ctx.structDeclarator().declarator();
    } else {
      Seq(ctx.structDeclarator().declarator());
    }

  def ctypesOf(ctx : CParser.StructDeclarationContext) : Seq[CType] = {
    val specrs = flattenStructDeclarationSpecifiers(ctx.specifierQualifierList())
    val specifiedType = ctypeFromSpecifiers(specrs);

    declaratorsOfStructDeclaratorList(ctx.structDeclaratorList()).map(ctypeOfDeclarator(specifiedType, _));
  }

  def ctypesOf(ctx : CParser.StructDeclarationListContext) : Seq[CType] =
    if (ctx.structDeclarationList() != null) {
      ctypesOf(ctx.structDeclarationList()) ++ ctypesOf(ctx.structDeclaration());
    } else {
      ctypesOf(ctx.structDeclaration());
    }

  def ctypeOf(ctx : CParser.ParameterDeclarationContext) : CType =
    if (ctx.declarator() != null) {
      val specifiedType = ctypeOf(ctx.declarationSpecifiers());
      ctypeOfDeclarator(specifiedType, ctx.declarator());
    } else {
      // Abstract parameter declaration
      val specifiedType = ctypeFromSpecifiers(flattenDeclarationSpecifiers(ctx.declarationSpecifiers2().declarationSpecifier()));

      if (ctx.abstractDeclarator() != null) {
        ctypeOf(specifiedType, ctx.abstractDeclarator());
      } else {
        specifiedType;
      }
    }

  def ctypesOf(ctx : CParser.ParameterListContext) : Seq[CType] =
    if (ctx.parameterList() != null) {
      ctypesOf(ctx.parameterList()) :+ ctypeOf(ctx.parameterDeclaration());
    } else {
      Seq(ctypeOf(ctx.parameterDeclaration()));
    }

  def ctypesOf(ctx : CParser.ParameterTypeListContext) : Seq[CType] =
    if (ctx.getChild(1) != null) {
      ctypesOf(ctx.parameterList()) :+ VarArgType();
    } else {
      ctypesOf(ctx.parameterList())
    }

  def ctypeOfStructOrUnionSpecifier(ctx : CParser.StructOrUnionSpecifierContext) : StructType =
    if (ctx.structDeclarationList() != null) {
      // in the form of "struct Identifier? { structDeclList };",
      // (null for anonymous struct).
      val structTag = if (ctx.Identifier() != null) ctx.Identifier().getText() else null;

      val members = ctypesOf(ctx.structDeclarationList());

      StructType(null, structTag, members.toSeq);
    } else {
      val structTag = ctx.Identifier().getText();
      declaredStructs.get(structTag) match {
        case Some(struct) => struct;
        case None => throw new RuntimeException(s"struct $structTag undeclared!");
      }
    }

  override def enterStructOrUnionSpecifier(ctx : CParser.StructOrUnionSpecifierContext) {
    if (ctx.structDeclarationList() != null) {
      val structTag = if (ctx.Identifier() != null) ctx.Identifier().getText() else null;

      if (structTag != null) {
        val struct = ctypeOfStructOrUnionSpecifier(ctx);
        declaredStructs += structTag -> struct;
      }
    }
  }

  def flattenDeclarationSpecifiers(specsCtx : Seq[CParser.DeclarationSpecifierContext]) : Seq[RuleContext] =
    specsCtx.map({ specifier =>
      if (specifier.typeSpecifier() != null) {
        Some(specifier.typeSpecifier()); // Take the typeSpecifiers
      } else {
        None; // Ignore everything else.
      }
    }).flatten;

  def ctypeFromSpecifiers(specs : Seq[RuleContext]) : CType =
    // Mostly this is just grab the typedef, and turn it into a string?
    // typeSpecifier: int/float/etc., typedef'd e.g. myInt, structs/unions,
    specs.map({ x =>
      x match {
        case primCtx : CParser.TypeSpecifierPrimitiveContext =>
          PrimitiveType(null, primCtx.getText());
        case typedefCtx : CParser.TypeSpecifierTypedefContext => {
          val label = x.getText();

          declaredTypedefs.get(label) match {
            case Some(ctype) => ctype;
            case None => throw new RuntimeException(s"typedef $label undeclared");
          }
        }
        case enumSpecCtx : CParser.TypeSpecifierEnumContext =>
          ctypeOfEnumSpecifier(enumSpecCtx.enumSpecifier());
        case structSpecCtx : CParser.TypeSpecifierStructOrUnionContext =>
          ctypeOfStructOrUnionSpecifier(structSpecCtx.structOrUnionSpecifier());
        case _ =>
          throw new UnsupportedOperationException("Unsupported Type Specifier");
      }
    }).foldLeft(VoidType() : CType)({ (ct1, ct2) =>
      ct1 match {
        case PrimitiveType(_, pt1) => {
          ct2 match {
            // "Merge" the two PrimitiveTypes together
            case PrimitiveType(_, pt2) => PrimitiveType(null, pt1 + " " + pt2);
            case _ => throw new UnsupportedOperationException("Cannot 'merge' these type specificers.");
          }
        }
        // In most cases, list of type specifiers will just be just one.
        case _ => ct2;
      }
    });

  // The CType we derive from declnSpecrs won't have an ID, etc.
  def ctypeOf(specsCtx : CParser.DeclarationSpecifiersContext) : CType = {
    val typeSpecrs = flattenDeclarationSpecifiers(specsCtx.declarationSpecifier());
    return ctypeFromSpecifiers(typeSpecrs);
  }

  def idOfDeclarator(ctx : CParser.DeclaratorContext) : String = {
    ctx.directDeclarator().getText();
  }

  def ctypeOfDirectDeclarator(specifiedType : CType, dirDeclrCtx : CParser.DirectDeclaratorContext) : CType =
    dirDeclrCtx match {
      case ctx : CParser.DeclaredIdentifierContext => {
        val id = ctx.getText();

        // Might be a typedef, which we need to track.
        if (isInDeclarationContextWithTypedef(ctx)) {
          declaredTypedefs += id -> specifiedType;
        }

        fixCType(specifiedType, id);
      }
      case ctx : CParser.DeclaredParenthesesContext =>
        ctypeOfDeclarator(specifiedType, ctx.declarator());
      case ctx : CParser.DeclaredArrayContext => {
        val n = if (ctx.assignmentExpression() != null) {
          rewriter.getText(ctx.assignmentExpression().getSourceInterval());
        } else {
          // declared array might not have size; e.g. arguments for functions.
          // e.g. *args[].
          null;
        }

        val arrType = ArrayType(null, null, n, specifiedType);
        ctypeOfDirectDeclarator(arrType, ctx.directDeclarator());
      }
      case ctx : CParser.DeclaredFunctionPrototypeContext => {
        val paramTypes = ctypesOf(ctx.parameterTypeList());
        val fnType = FunctionType(null, specifiedType, paramTypes);

        ctypeOfDirectDeclarator(fnType, ctx.directDeclarator());
      }
      case ctx : CParser.DeclaredFunctionDefinitionContext =>
        throw new RuntimeException("TODO: Old-style function definitions");
    }

  def ctypeOfAbstractDirectDeclarator(specifiedType : CType, abstrDeclrCtx : CParser.DirectAbstractDeclaratorContext) : CType =
    abstrDeclrCtx match {
      case ctx : CParser.AbstractDeclaredParenthesesContext =>
        ctypeOf(specifiedType, ctx.abstractDeclarator());
      case ctx : CParser.AbstractDeclaredArrayContext => {
        val n = if (ctx.assignmentExpression() != null) {
          rewriter.getText(ctx.assignmentExpression().getSourceInterval());
        } else {
          // declared array might not have size; e.g. arguments for functions.
          // e.g. *args[].
          null;
        }

        val arrType = ArrayType(null, null, n, specifiedType);
        ctypeOfAbstractDirectDeclarator(arrType, ctx.directAbstractDeclarator());
      }
      case ctx : CParser.AbstractDeclaredFunctionPrototypeContext =>
        throw new RuntimeException("TODO: Abstract fun proto.");
    }

  def ctypeOf(specifiedType : CType, ctx : CParser.AbstractDeclaratorContext) : CType = {
    if (ctx.directAbstractDeclarator() != null) {
      val ptype = pointerTypeOfDeclaredType(specifiedType, ctx.pointer());
      ctypeOfAbstractDirectDeclarator(ptype, ctx.directAbstractDeclarator());
    } else {
      pointerTypeOfDeclaredType(specifiedType, ctx.pointer());
    }
  }

  def ctypeOfDeclarator(specifiedType : CType, declrCtx : CParser.DeclaratorContext) : CType = {
    val ptype = pointerTypeOfDeclaredType(specifiedType, declrCtx.pointer());
    val declaredType = ctypeOfDirectDeclarator(ptype, declrCtx.directDeclarator());

    return declaredType;
  }

  def ctypeOf(specsCtx : CParser.DeclarationSpecifiersContext, declrCtx : CParser.DeclaratorContext) : CType = {
    val specifiedType = ctypeOf(specsCtx);
    return ctypeOfDeclarator(specifiedType, declrCtx);
  }

  def ctypeOf(specsCtx : CParser.DeclarationSpecifiersContext, initDeclrCtx : CParser.InitDeclaratorContext) : CType = {
    // Check initializer; may need it for Arrays
    val ct = ctypeOf(specsCtx, initDeclrCtx.declarator());

    return ct;
  }

  override def exitDeclaration(ctx : CParser.DeclarationContext) {
    if (ctx.initDeclaratorList() != null) {
      // Derive what CTypes we can from the declaration.
      val initDeclrs = listOfInitDeclrList(ctx.initDeclaratorList());

      // For each, call to ctypeOf
      val declnCTypes = initDeclrs.map(ctypeOf(ctx.declarationSpecifiers(), _));
      allCTypes = allCTypes ++ declnCTypes;
    }
  }

  override def exitFunctionDefinition(ctx : CParser.FunctionDefinitionContext) {
    val specifiedType = ctypeOf(ctx.declarationSpecifiers());
    val definedFun = ctypeOfDeclarator(specifiedType, ctx.declarator())

    allCTypes = allCTypes :+ definedFun;
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