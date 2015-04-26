package edu.nus.worksheet.instrumentor

import org.antlr.v4.runtime._
import org.antlr.v4.runtime.tree._
import scala.collection.JavaConversions._
import edu.nus.worksheet.instrumentor.Util.getANTLRLexerTokensParserFor;
import edu.nus.worksheet.instrumentor.Util.lookup;
import edu.nus.worksheet.instrumentor.Util.commonRealType;
import edu.nus.worksheet.instrumentor.Util.isIntType;
import edu.nus.worksheet.instrumentor.Util.isArithmeticType;
import edu.nus.worksheet.instrumentor.CTypeToDeclaration.declarationOf;
import edu.nus.worksheet.instrumentor.CTypeToDeclaration.stringOfTypeName;

// Return a CType from some expression.
//
// CType returned doesn't necessarily have an id, but it might.
//
// When scope is 'typesafe' with the AST nodes which this instance visits,
//  then CType (should) always be returned.
// Otherwise, null may be returned.
// e.g. asking for type of (undeclared) variable would return null.
//
// Need StringConstruction to get CTypes.
// (StringConstruction needs tokens for getting the size of an array,
//  which we could work-around with AST -> CType -> String).
class TypeInference(scopes : ParseTreeProperty[Scope], ctypeFromDecl : CTypeFromDeclaration) extends CBaseVisitor[CType] {
  override def visitConstInteger(ctx : CParser.ConstIntegerContext) : CType =
    PrimitiveType(ctx.getText(), "int");
  
  override def visitConstFloat(ctx : CParser.ConstFloatContext) : CType =
    PrimitiveType(ctx.getText(), "double");

  override def visitConstChar(ctx : CParser.ConstCharContext) : CType =
    PrimitiveType(ctx.getText(), "char");
  
  private[TypeInference] def changeCTypeId(ct : CType, newId : String) =
    ct.fId({ _ => newId });

  override def visitPrimaryId(ctx : CParser.PrimaryIdContext) : CType = {
    val id = ctx.Identifier().getText();
    return lookup(scopes, ctx, id) match {
      case Some(ct) =>
        changeCTypeId(ct, id);
      // Type-of an undefined variable? Compiler Error!
      case None => throw new RuntimeException(s"Could not find var $id in scope!");
    }
  }

  override def visitPrimaryConst(ctx : CParser.PrimaryConstContext) : CType =
    visit(ctx.constant());

  override def visitPrimaryString(ctx : CParser.PrimaryStringContext) : CType = {
    def stripQuote(s : TerminalNode) : String =
      s.getText().substring(1, s.getText().length() - 1);
    val text = '"' + ctx.StringLiteral().map(stripQuote _).mkString + '"';
    return PrimitiveType(text, "string");
  }

  override def visitPrimaryParen(ctx : CParser.PrimaryParenContext) : CType = {
    val ct = visit(ctx.expression());
    if (ct.id.startsWith("(*") && ct.id.endsWith(")")) {
      // Parenthesised expressions, e.g. from Pointer..
      // Don't add another pair of parentheses.
      return ct;
    } else {
      return changeCTypeId(ct, s"(${ct.id})");
    }
  }

  override def visitPostfixFallthrough(ctx : CParser.PostfixFallthroughContext) : CType =
    visit(ctx.primaryExpression());

  override def visitPostfixArray(ctx : CParser.PostfixArrayContext) : CType =
    visit(ctx.postfixExpression()) match {
      // pfxArray is like arr[idx]
      case ArrayType(arrId, _, _, of) => {
        val idxT = visit(ctx.expression());
        val idx = idxT.id;
        changeCTypeId(of, s"${arrId}[$idx]");
      }
      case t => throw new RuntimeException(s"Expected to get an ArrayType, but got $t.");
    }

  def inferArgumentTypes(ctx : CParser.ArgumentExpressionListContext) : Seq[CType] =
    if (ctx.argumentExpressionList() != null) {
      inferArgumentTypes(ctx.argumentExpressionList()) :+ visit(ctx.assignmentExpression());
    } else {
      Seq(visit(ctx.assignmentExpression()));
    }

  override def visitPostfixCall(ctx : CParser.PostfixCallContext) : CType = {
    val (fname, rtnType) = visit(ctx.postfixExpression()) match {
      case FunctionType(f, rtnType, _) => (f, rtnType);
      case PointerType(f, FunctionType(_, rtnType, _)) => (f, rtnType);
      case t => throw new RuntimeException(s"Expected to get a Function or Pointer Type, but got $t.");
    }
    val argTypes = if (ctx.argumentExpressionList() != null) inferArgumentTypes(ctx.argumentExpressionList()) else Seq();
    val fCallString = fname + "(" + argTypes.map({ ct => ct.id }).mkString(",") + ")";

    rtnType match {
      case PrimitiveType(id, "void") => null;
      case _ => changeCTypeId(rtnType, fCallString);
    }
  }

  override def visitPostfixStruct(ctx : CParser.PostfixStructContext) : CType = {
    val pfxExpr = ctx.postfixExpression();
    val memberId = ctx.Identifier().getText();

    val structType = visit(pfxExpr) match {
      case s : StructType => s;
      case s => throw new RuntimeException(s"Expected StructType, got $s");
    }

    structType.getMember(memberId) match {
      case Some(ct) => ct;
      case None => throw new RuntimeException(s"Couldn't find member $memberId in struct $structType");
    }
  }

  override def visitPostfixPtrStruct(ctx : CParser.PostfixPtrStructContext) : CType = {
    val pfxExpr = ctx.postfixExpression();
    val memberId = ctx.Identifier().getText();

    val derefStruct = visit(pfxExpr) match {
      case PointerType(_, s : StructType) => s;
      case s => throw new RuntimeException(s"Expected StructType, got $s");
    }

    derefStruct.getMember(memberId) match {
      case Some(ct) => ct;
      case None => throw new RuntimeException(s"Couldn't find member $memberId in struct $derefStruct");
    }
  }

  override def visitPostfixIncr(ctx : CParser.PostfixIncrContext) : CType = {
    val ct = visit(ctx.postfixExpression());
    changeCTypeId(ct, ct.id + ctx.getChild(1).getText());
  }

  private[TypeInference] def stringOf(ctx : CParser.DesignationContext) : String = {
    def asList(desigListCtx : CParser.DesignatorListContext) : Seq[CParser.DesignatorContext] =
      if (desigListCtx.designatorList() != null) {
        asList(desigListCtx.designatorList()) :+ desigListCtx.designator();
      } else {
        Seq(desigListCtx.designator());
      }

    asList(ctx.designatorList()).map({ x =>
      x match {
        case diCtx : CParser.DesignatorIndexContext => {
          val ct = visit(diCtx.constantExpression());
          "[" + ct.id + "]";
        }
        case dmCtx : CParser.DesignatorMemberContext =>
          "." + dmCtx.Identifier().getText();
      }
    }).mkString + " = ";
  }

  private[TypeInference] def stringOf(desigCtx : CParser.DesignationContext, initrCtx : CParser.InitializerContext) : String = {
    val designStr = if (desigCtx != null) stringOf(desigCtx) else "";
    val initrStr = initrCtx match {
      case aeInitr : CParser.InitializerAssgExprContext =>
        visit(aeInitr.assignmentExpression()).id;
      case initrInitr : CParser.InitializerInitListContext =>
        "{ " + stringOf(initrInitr.initializerList()) + " }";
    };

    designStr + initrStr;
  }

  private[TypeInference] def stringOf(ctx : CParser.InitializerListContext) : String =
    if (ctx.initializerList() != null) {
      stringOf(ctx.initializerList()) + ", " + stringOf(ctx.designation(), ctx.initializer());
    } else {
      stringOf(ctx.designation(), ctx.initializer());
    }

  override def visitPostfixCompoundLiteral(ctx : CParser.PostfixCompoundLiteralContext) : CType = {
    val typeNameType = ctypeFromDecl.ctypeOf(ctx.typeName());
    val ct = changeCTypeId(typeNameType, s"(${stringOfTypeName(typeNameType)}) { ${stringOf(ctx.initializerList())} }");

    ct match {
      case at : ArrayType => at.coerceToPointerType();
      case _ => ct;
    }
  }


  override def visitUnaryFallthrough(ctx : CParser.UnaryFallthroughContext) : CType =
    visit(ctx.postfixExpression());

  override def visitUnaryIncr(ctx : CParser.UnaryIncrContext) : CType = {
    val ct = visit(ctx.unaryExpression());
    changeCTypeId(ct, ctx.getChild(0).getText() + ct.id);
  }

  override def visitUnaryOpExpr(ctx : CParser.UnaryOpExprContext) : CType = {
    val unOp = ctx.unaryOperator();
    val castExpr = ctx.castExpression();
    val castExprT = visit(castExpr);

    def prefixWithOp(s : String) : String =
      unOp.getText() + s;

    unOp.getText() match {
      case "&" =>
        PointerType("&" + castExprT.id, castExprT);
      case "*" => // deref
        castExprT match {
          case PointerType(_, of) => of;
          case _ => throw new RuntimeException(s"Cannot infer dereference type: not a pointer: $castExprT");
        }
      case "+" => castExprT.fId(prefixWithOp);
      case "-" => castExprT.fId(prefixWithOp);
      case "~" => castExprT.fId(prefixWithOp);
      case "!" => castExprT.fId(prefixWithOp);
    }
  }

  override def visitUnarySizeofExpr(ctx : CParser.UnarySizeofExprContext) : CType = {
    val ct = visit(ctx.unaryExpression());
    PrimitiveType("sizeof " + ct.id, "int"); // actually `long unsigned int`
  }

  override def visitUnarySizeofType(ctx : CParser.UnarySizeofTypeContext) : CType = {
    val ct = ctypeFromDecl.ctypeOf(ctx.typeName());
    PrimitiveType("sizeof(" + ct.id + ")", "int");
  }


  override def visitCastFallthrough(ctx : CParser.CastFallthroughContext) : CType =
    visit(ctx.unaryExpression());

  override def visitCastExpr(ctx : CParser.CastExprContext) : CType = {
    val typeNameCt = ctypeFromDecl.ctypeOf(ctx.typeName());
    val ct = visit(ctx.castExpression());
    changeCTypeId(typeNameCt, s"(${stringOfTypeName(typeNameCt)}) ${ct.id}");
  }

  private[TypeInference] def commonArithmeticType(ct1 : CType, ct2 : CType) : CType = {
    val t1 = ct1 match {
      case PrimitiveType(_, t) => t;
      case _ => throw new Exception("Invalid type for arithmetic operand; must be primitive type");
    }
    val t2 = ct2 match {
      case PrimitiveType(_, t) => t;
      case _ => throw new Exception("Invalid type for arithmetic operand; must be primitive type");
    }

    // Assumes arithmetic only for real types.
    PrimitiveType(null, commonRealType(t1, t2));
  }

  private[TypeInference] def ctypeOfCommonArithmetic(operator : String,
                                                     operand1 : ParserRuleContext,
                                                     operand2 : ParserRuleContext) : CType = {
      val ct1 = visit(operand1);
      val ct2 = visit(operand2);

      changeCTypeId(commonArithmeticType(ct1, ct2),
                    ct1.id + " " + operator + " " + ct2.id);
  }

  private[TypeInference] def ctypeOfBooleanExpression(operator : String,
                                                      operand1 : ParserRuleContext,
                                                      operand2 : ParserRuleContext) : CType = {
      val ct1 = visit(operand1);
      val ct2 = visit(operand2);

      PrimitiveType(ct1.id + " " + operator + " " + ct2.id, "int");
  }

  override def visitMultFallthrough(ctx : CParser.MultFallthroughContext) : CType =
    visit(ctx.castExpression());

  override def visitMultExpr(ctx : CParser.MultExprContext) : CType = {
    val op = ctx.getChild(1).getText();
    val ctx1 = ctx.multiplicativeExpression();
    val ctx2 = ctx.castExpression();

    ctypeOfCommonArithmetic(op, ctx1, ctx2);
  }

  override def visitAddFallthrough(ctx : CParser.AddFallthroughContext) : CType =
    visit(ctx.multiplicativeExpression());

  override def visitAddExpr(ctx : CParser.AddExprContext) : CType = {
    val op = ctx.getChild(1);
    val ct1 = visit(ctx.additiveExpression());
    val ct2 = visit(ctx.multiplicativeExpression());

    val nId = ct1.id + " " + op.getText() + " " + ct2.id;

    return op.getText() match {
      case "+" => {
        // Addition of pointer with int.
        if (ct1.isInstanceOf[PointerType] && isIntType(ct2)) {
          changeCTypeId(ct1, s"($nId)");
        } else if (ct2.isInstanceOf[PointerType] && isIntType(ct1)) {
          changeCTypeId(ct2, s"($nId)");
        } else {
          // Arithmetic addition.
          // Assume ct1, ct2 are arithmetic.

          changeCTypeId(commonArithmeticType(ct1, ct2), nId);
        }
      }
      case "-" => {
        // operands are either:
        //    both arithmetic
        // OR both pointers to "compatible object types"
        // OR left is pointer, right is integer
        if (ct1.isInstanceOf[PointerType] && ct2.isInstanceOf[PointerType]) {
          // Pointer difference
          // Assume ct1, ct2 compatible
          PrimitiveType(nId, "ptrdiff_t");
        } else if (ct1.isInstanceOf[PointerType] && isIntType(ct2)) {
          changeCTypeId(ct1, nId);
        } else {
          // Assumes ct1, ct2 are arithmetic

          changeCTypeId(commonArithmeticType(ct1, ct2), nId);
        }
      }
      case _ => throw new IllegalStateException("Additive operator must be '+' or '-'");
    }
  }

  override def visitShiftFallthrough(ctx : CParser.ShiftFallthroughContext) : CType =
    visit(ctx.additiveExpression());

  override def visitShiftExpr(ctx : CParser.ShiftExprContext) : CType = {
      val op = ctx.getChild(1).getText();
      val ctx1 = ctx.shiftExpression();
      val ctx2 = ctx.additiveExpression();

      ctypeOfCommonArithmetic(op, ctx1, ctx2);
  }

  // For relational and equality expression,
  // we assume that the constraints on the types of operands holds.
  // The type of the expression doesn't otherwise depend on the operands.

  override def visitRelFallthrough(ctx : CParser.RelFallthroughContext) : CType =
    visit(ctx.shiftExpression());

  override def visitRelExpr(ctx : CParser.RelExprContext) : CType = {
    val op = ctx.getChild(1).getText();
    val ctx1 = ctx.relationalExpression();
    val ctx2 = ctx.shiftExpression();

    ctypeOfBooleanExpression(op, ctx1, ctx2);
  }

  override def visitEqFallthrough(ctx : CParser.EqFallthroughContext) : CType =
    visit(ctx.relationalExpression());

  override def visitEqExpr(ctx : CParser.EqExprContext) : CType = {
    val op = ctx.getChild(1).getText();
    val ctx1 = ctx.equalityExpression();
    val ctx2 = ctx.relationalExpression();

    ctypeOfBooleanExpression(op, ctx1, ctx2);
  }

  override def visitAndFallthrough(ctx : CParser.AndFallthroughContext) : CType =
    visit(ctx.equalityExpression());

  override def visitAndExpr(ctx : CParser.AndExprContext) : CType = {
    val op = ctx.getChild(1).getText();
    val ctx1 = ctx.andExpression();
    val ctx2 = ctx.equalityExpression();

    ctypeOfCommonArithmetic(op, ctx1, ctx2);
  }

  override def visitXorFallthrough(ctx : CParser.XorFallthroughContext) : CType =
    visit(ctx.andExpression());

  override def visitXorExpr(ctx : CParser.XorExprContext) : CType = {
    val op = ctx.getChild(1).getText();
    val ctx1 = ctx.exclusiveOrExpression();
    val ctx2 = ctx.andExpression();

    ctypeOfCommonArithmetic(op, ctx1, ctx2);
  }

  override def visitOrFallthrough(ctx : CParser.OrFallthroughContext) : CType =
    visit(ctx.exclusiveOrExpression());

  override def visitOrExpr(ctx : CParser.OrExprContext) : CType = {
    val op = ctx.getChild(1).getText();
    val ctx1 = ctx.inclusiveOrExpression();
    val ctx2 = ctx.exclusiveOrExpression();

    ctypeOfCommonArithmetic(op, ctx1, ctx2);
  }

  override def visitLogAndFallthrough(ctx : CParser.LogAndFallthroughContext) : CType =
    visit(ctx.inclusiveOrExpression());

  override def visitLogAndExpr(ctx : CParser.LogAndExprContext) : CType = {
    val op = ctx.getChild(1).getText();
    val ctx1 = ctx.logicalAndExpression();
    val ctx2 = ctx.inclusiveOrExpression();

    ctypeOfBooleanExpression(op, ctx1, ctx2);
  }

  override def visitLogOrFallthrough(ctx : CParser.LogOrFallthroughContext) : CType =
    visit(ctx.logicalAndExpression());

  override def visitLogOrExpr(ctx : CParser.LogOrExprContext) : CType = {
    val op = ctx.getChild(1).getText();
    val ctx1 = ctx.logicalOrExpression();
    val ctx2 = ctx.logicalAndExpression();

    ctypeOfBooleanExpression(op, ctx1, ctx2);
  }

  // Conditional Expression is surprisingly involved.
  override def visitConditionalExpression(ctx : CParser.ConditionalExpressionContext) : CType = {
    if (ctx.conditionalExpression() == null) {
      visit(ctx.logicalOrExpression());
    } else {
      // _ ? ct1 : ct2;
      val condT = visit(ctx.logicalOrExpression());
      val ct1 = visit(ctx.expression());
      val ct2 = visit(ctx.conditionalExpression());

      val nId = s"${condT.id} ? ${ct1.id} : ${ct2.id}";

      val ct = if (isArithmeticType(ct1) && isArithmeticType(ct2)) {
        commonArithmeticType(ct1, ct2);
      } else {
        // Various cases here, but let's assume they're of same/compatible types.
        ct1;
      };

      changeCTypeId(ct, nId);
    }
  }

  override def visitAssgFallthrough(ctx : CParser.AssgFallthroughContext) : CType =
    visit(ctx.conditionalExpression());

  override def visitAssgExpr(ctx : CParser.AssgExprContext) : CType = {
    val op = ctx.getChild(1).getText();
    val ct1 = visit(ctx.unaryExpression());
    val ct2 = visit(ctx.assignmentExpression());

    val nId = ct1.id + " " + op + " " + ct2.id;

    val ct = op match {
      case "=" =>
        // Simple assignment, result is (unqualified) type of unaryExpr.
        ct1;
      case "+=" => {
        if (ct1.isInstanceOf[PointerType] && isIntType(ct2)) {
          ct1
        } else {
          commonArithmeticType(ct1, ct2);
        }
      }
      case "-=" => {
        if (ct1.isInstanceOf[PointerType] && isIntType(ct2)) {
          ct1
        } else {
          commonArithmeticType(ct1, ct2);
        }
      }
      case _ => commonArithmeticType(ct1, ct2);
    }

    changeCTypeId(ct, nId);
  }

  override def visitExprFallthrough(ctx : CParser.ExprFallthroughContext) : CType =
    visit(ctx.assignmentExpression());

  override def visitCommaExpr(ctx : CParser.CommaExprContext) : CType = {
    val ct1 = visit(ctx.expression());
    val ct2 = visit(ctx.assignmentExpression());

    val nId = ct1.id + ", " + ct2.id;

    changeCTypeId(ct2, nId);
  }

  // Sigh. ANTLR visitors, must explicitly visit last rule
  override def visitTypeInferenceFixture(ctx : CParser.TypeInferenceFixtureContext) : CType =
    visit(ctx.expression());
}

object TypeInference {
  def inferType(program : String, of : String) : CType = {
    val in = (if (program != null) program + ";" else "") + of;
    val (lexer, tokens, parser) = getANTLRLexerTokensParserFor(in);

    val walker = new ParseTreeWalker();
    val tree = parser.typeInferenceFixture(); // translationUnit + expression

    val defineScopesPhase = new DefineScopesPhase();
    walker.walk(defineScopesPhase, tree);
    val scopes = defineScopesPhase.scopes;

    val strCons = new StringConstruction(scopes);
    walker.walk(strCons, tree);

    // Need to clean up any forward declarations.
    defineScopesPhase.allScopes.foreach(_.flattenForwardDeclarations());


    val ctypeFromDecl = new CTypeFromDeclaration(scopes);
    val tooler = new TypeInference(scopes, ctypeFromDecl);

    return tooler.visit(tree);
  }

  def p_inferType(program : String, of : String) : CType = {
    println("TypeInfer " + of);
    val t = inferType(program, of);
    println(t);
    println();
    return t;
  }

  def main(args : Array[String]) : Unit = {
    p_inferType(null, "5");
    p_inferType(null, "5.34");
    p_inferType(null, "'x'");

    p_inferType(null, "\"Abc\"");
    p_inferType(null, "\"Abc\" \"def\"");
    p_inferType(null, "(5)");
    p_inferType(("int x;"), "x");

    p_inferType(("int x[2] = {1,2};"), "x[0]");
    p_inferType(("struct {int x;} s;"), "s.x");
    p_inferType(("struct S {int x;} s; struct S *p = &s;"), "p->x");
    p_inferType(("int i;"), "i++");
    println("Done");
  }
}