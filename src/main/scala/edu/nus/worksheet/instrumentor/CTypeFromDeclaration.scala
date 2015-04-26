package edu.nus.worksheet.instrumentor

import org.antlr.v4.runtime._
import org.antlr.v4.runtime.tree._
import scala.collection.JavaConversions._
import scala.collection.immutable.List
import scala.collection.mutable.Stack;
import scala.collection.mutable.Map;
import edu.nus.worksheet.instrumentor.Util.currentScopeForContext;

class CTypeFromDeclaration(scopes : ParseTreeProperty[Scope]) {

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
        case VoidType() => PointerType(null, null); // cannot output void-ptr.
        case ptr : PointerType => PointerType(null, null); // Discard 'of' for ptr-to-ptr.
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
      currentScopeForContext(ctx, scopes).resolveEnum(enumTag) match {
        case Some(enum) => enum;
        case None => throw new RuntimeException(s"struct $enumTag undeclared!");
      }
    }

  def ctypeOf(ctx : CParser.TypeNameContext) : CType = {
    val specrs = listOfStructDeclarationSpecifiers(ctx.specifierQualifierList())
    val specifiedType = ctypeFromSpecifiers(specrs);

    if (ctx.abstractDeclarator() != null) {
      ctypeOf(specifiedType, ctx.abstractDeclarator());
    } else {
      specifiedType;
    }
  }

  def listOfStructDeclarationSpecifiers(specsCtx : CParser.SpecifierQualifierListContext) : Seq[RuleContext] = {
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
    val specrs = listOfStructDeclarationSpecifiers(ctx.specifierQualifierList())
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
      val specifiedType = ctypeFromSpecifiers(listOfDeclarationSpecifiers(ctx.declarationSpecifiers2().declarationSpecifier()));

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

  def ctypeOfStructOrUnionSpecifier(ctx : CParser.StructOrUnionSpecifierContext) : CType =
    if (ctx.structDeclarationList() != null) {
      // in the form of "struct Identifier? { structDeclList };",
      // (null for anonymous struct).
      val structOrUnion = ctx.structOrUnion().getText();
      val structTag = if (ctx.Identifier() != null) ctx.Identifier().getText() else null;

      val members = ctypesOf(ctx.structDeclarationList());

      StructType(null, structOrUnion, structTag, members.toSeq);
    } else {
      val structTag = ctx.Identifier().getText();
      currentScopeForContext(ctx, scopes).resolveStruct(structTag) match {
        case Some(struct) => struct;
        case None => {
          // Could be forward declaration here.
          // We'll assume it is.
          // (The other case is using a tag of undeclared struct).
          ForwardDeclarationType(null, structTag, currentScopeForContext(ctx, scopes));
        }
      }
    }

  def listOfDeclarationSpecifiers(specsCtx : Seq[CParser.DeclarationSpecifierContext]) : Seq[RuleContext] =
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

          currentScopeForContext(typedefCtx, scopes).resolveTypedef(label) match {
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
    val typeSpecrs = listOfDeclarationSpecifiers(specsCtx.declarationSpecifier());
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
          currentScopeForContext(ctx, scopes).defineTypedef(id, specifiedType);
        }

        StringConstruction.fixCType(specifiedType, id);
      }
      case ctx : CParser.DeclaredParenthesesContext =>
        ctypeOfDeclarator(specifiedType, ctx.declarator());
      case ctx : CParser.DeclaredArrayContext => {
        val n = if (ctx.assignmentExpression() != null) {
          val typeInfer = new TypeInference(scopes, this);
          typeInfer.visit(ctx.assignmentExpression()).id;
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
      case ctx : CParser.DeclaredFunctionDefinitionContext => {
        // K&R style function declaration/definition
        // Don't need to worry about identifier list..
        val paramTypes = Seq();
        val fnType = FunctionType(null, specifiedType, paramTypes);

        ctypeOfDirectDeclarator(fnType, ctx.directDeclarator());
      }
    }

  def ctypeOfAbstractDirectDeclarator(specifiedType : CType, abstrDeclrCtx : CParser.DirectAbstractDeclaratorContext) : CType =
    abstrDeclrCtx match {
      case ctx : CParser.AbstractDeclaredParenthesesContext =>
        ctypeOf(specifiedType, ctx.abstractDeclarator());
      case ctx : CParser.AbstractDeclaredArrayContext => {
        val n = if (ctx.assignmentExpression() != null) {
          val typeInfer = new TypeInference(scopes, this);
          typeInfer.visit(ctx.assignmentExpression()).id;
        } else {
          // declared array might not have size; e.g. arguments for functions.
          // e.g. *args[].
          null;
        }

        val arrType = ArrayType(null, null, n, specifiedType);
        if (ctx.directAbstractDeclarator() != null) {
          ctypeOfAbstractDirectDeclarator(arrType, ctx.directAbstractDeclarator());
        } else {
          arrType;
        }
      }
      case ctx : CParser.AbstractDeclaredFunctionPrototypeContext => {
        val paramTypes = if (ctx.parameterTypeList() != null)
                           ctypesOf(ctx.parameterTypeList())
                         else
                           Seq();
        val fnType = FunctionType(null, specifiedType, paramTypes);

        if (ctx.directAbstractDeclarator() != null) {
          ctypeOfAbstractDirectDeclarator(fnType, ctx.directAbstractDeclarator());
        } else {
          fnType;
        }
      }
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
}