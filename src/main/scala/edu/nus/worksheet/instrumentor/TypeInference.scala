package edu.nus.worksheet.instrumentor

import org.antlr.v4.runtime._
import org.antlr.v4.runtime.tree._
import scala.collection.JavaConversions._
import edu.nus.worksheet.instrumentor.Util.commonRealType;
import edu.nus.worksheet.instrumentor.Util.isIntType;
import edu.nus.worksheet.instrumentor.Util.isArithmeticType;

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
class TypeInference(stringCons : StringConstruction) extends CBaseVisitor[CType] {
  override def visitConstant(ctx : CParser.ConstantContext) : CType = {
    val t : String = if(ctx.IntegerConstant() != null) {
        "int";
      } else if (ctx.FloatingConstant() != null) {
        "double";
      } else if (ctx.CharacterConstant() != null) {
        "char";
      } else {
        throw new Exception("constant must be Integer,Floating or Character Constant");
      }
    return PrimitiveType(ctx.getText(), t);
  }

  private[TypeInference] def flatReplaceId(ct : CType, newId : String) : CType =
    ct match {
      case PrimitiveType(_, t) => PrimitiveType(newId, t);
      case ArrayType(_, idx, n, of) => ArrayType(newId, idx, n, of);
      case PointerType(_, of) => PointerType(newId, of);
      case StructType(_, sOrU, tag, members) => StructType(newId, sOrU, tag, members);
      case EnumType(_, tag, constants) => EnumType(newId, tag, constants);
      case FunctionType(_, rtn, params) => FunctionType(newId, rtn, params);
      case _ => throw new UnsupportedOperationException("Cannot replace id (flatly) for " + ct);
    }

  private[TypeInference] def deepReplaceId(ct : CType, newId : String) : CType = {
    def replace(ct : CType) : CType =
      ct match {
        case PrimitiveType(_, t) => PrimitiveType(newId, t);
        case ArrayType(_, idx, n, of) => ArrayType(newId, idx, n, replace(of));
        case PointerType(_, of) => PointerType(newId, replace(of));
        case StructType(_, sOrU, tag, members) => StructType(newId, sOrU, tag, members);
        case EnumType(_, tag, constants) => EnumType(newId, tag, constants);
        case FunctionType(_, rtn, params) => FunctionType(newId, replace(rtn), params);
        case _ => throw new UnsupportedOperationException("Cannot replace id (flatly) for " + ct);
      }

    return replace(ct);
  }

  private[TypeInference] def changeCTypeId(ct : CType, newId : String) =
    ct.fId({ _ => newId });

  override def visitPrimaryExpression(ctx : CParser.PrimaryExpressionContext) : CType = {
    if (ctx.Identifier() != null) {
      val id = ctx.Identifier().getText();
      return stringCons.lookup(ctx, id) match {
        case Some(ct) =>
          changeCTypeId(ct, id);
        // Type-of an undefined variable? Compiler Error!
        case None => throw new RuntimeException(s"Could not find var $id in scope!");
      }
    } else if (ctx.constant() != null) {
      return visitConstant(ctx.constant());
    } else if (ctx.StringLiteral().length > 0) {
      def stripQuote(s : TerminalNode) : String =
        s.getText().substring(1, s.getText().length() - 1);
      val text = '"' + ctx.StringLiteral().map(stripQuote _).mkString + '"';
      return PrimitiveType(text, "string");
    } else if (ctx.expression() != null) {
      val ct = visitExpression(ctx.expression());
      if (ct.id.startsWith("(*") && ct.id.endsWith(")")) {
        // Parenthesised expressions, e.g. from Pointer..
        // Don't add another pair of parentheses.
        return ct;
      } else {
        return changeCTypeId(ct, s"(${ct.id})");
      }
    } else {
      throw new UnsupportedOperationException("Unknown/unsupported primaryExpression");
    }
  }

  override def visitPostfixFallthrough(ctx : CParser.PostfixFallthroughContext) : CType =
    visitPrimaryExpression(ctx.primaryExpression());

  override def visitPostfixArray(ctx : CParser.PostfixArrayContext) : CType =
    visit(ctx.postfixExpression()) match {
      // pfxArray is like arr[idx]
      case ArrayType(arrId, _, _, of) => {
        val idxT = visit(ctx.expression());
        val idx = idxT.id;
        changeCTypeId(of, s"${arrId}[$idx]");
      }
      case _ => null; // for some reason, didn't a proper type back.
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
      case _ => null; // for some reason, didn't get a proper type back
    }
    val argTypes = if (ctx.argumentExpressionList() != null) inferArgumentTypes(ctx.argumentExpressionList()) else Seq();
    val fCallString = fname + "(" + argTypes.map({ ct => ct.id }).mkString(",") + ")";
    changeCTypeId(rtnType, fCallString);
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
    initrCtx match {
      case aeInitr : CParser.InitializerAssgExprContext =>
        visit(aeInitr.assignmentExpression()).id;
      case initrInitr : CParser.InitializerInitListContext =>
        "{ " + stringOf(initrInitr.initializerList()) + " }";
    }
  }

  private[TypeInference] def stringOf(ctx : CParser.InitializerListContext) : String =
    if (ctx.initializerList() != null) {
      stringOf(ctx.initializerList()) + ", " + stringOf(ctx.designation(), ctx.initializer());
    } else {
      stringOf(ctx.designation(), ctx.initializer());
    }

  private[TypeInference] def stringOfTypeName(ct : CType) : String = {
    def specsDeclrOf(ct : CType) : (String, String) =
      ct match {
        case PrimitiveType(_, pt) =>
          (pt, "");
        case ArrayType(_, _, n, of) => {
          val (s, d) = specsDeclrOf(of);
          (s, s"$d[$n]");
        }
        case PointerType(_, of) => {
          val (s, d) = specsDeclrOf(of);
          (s, "(*)" + d);
        }
        case StructType(_, sOrU, tag, _) => {
          if (tag != null) {
            (s"$sOrU $tag", "");
          } else {
            throw new UnsupportedOperationException("Cannot handle typeName for anonymous struct");
          }
        }
        case EnumType(_, tag, _) => {
          if (tag != null) {
            (s"enum $tag", "");
          } else {
            throw new UnsupportedOperationException("Cannot handle typeName for anonymous enum");
          }
        }
        case FunctionType(_, rtnType, params) => {
          val (rtnS, rtnD) = specsDeclrOf(rtnType);
          val paramS = "(" + params.map(stringOfTypeName(_)).mkString(",") + ")";
          (rtnS, rtnD + paramS);
        }
        case _ => throw new UnsupportedOperationException(s"Cannot give string of type $ct")
      }

    val (s, d) = specsDeclrOf(ct);
    s + (if (!d.isEmpty()) " " + d; else "");
  }

  override def visitPostfixCompoundLiteral(ctx : CParser.PostfixCompoundLiteralContext) : CType = {
    val ct = stringCons.ctypeOf(ctx.typeName());
    changeCTypeId(ct, s"(${stringOfTypeName(ct)}) { ${stringOf(ctx.initializerList())} }");
  }


  override def visitUnaryFallthrough(ctx : CParser.UnaryFallthroughContext) : CType =
    visit(ctx.postfixExpression());

  override def visitUnaryIncr(ctx : CParser.UnaryIncrContext) : CType =
    visit(ctx.unaryExpression());

  override def visitUnaryOpExpr(ctx : CParser.UnaryOpExprContext) : CType = {
    val unOp = ctx.unaryOperator();
    val castExpr = ctx.castExpression();
    val castExprT = visitCastExpression(castExpr);

    return unOp.getText() match {
      case "&" => PointerType(null, castExprT);
      case "*" => // deref
        castExprT match {
          case PointerType(_, of) => of;
          case _ => null; // for some reason, didn't get a proper type back.
        }
      case "+" => castExprT;
      case "-" => castExprT;
      case "~" => castExprT;
      case "!" => castExprT;
    }
  }

  override def visitUnarySizeof(ctx : CParser.UnarySizeofContext) : CType =
    PrimitiveType(null, "size_t");


  override def visitCastExpression(ctx : CParser.CastExpressionContext) : CType =
    if (ctx.unaryExpression() != null) {
      visit(ctx.unaryExpression());
    } else {
      stringCons.ctypeOf(ctx.typeName());
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

  override def visitMultiplicativeExpression(ctx : CParser.MultiplicativeExpressionContext) : CType = {
    if (ctx.multiplicativeExpression() == null) {
      return visitCastExpression(ctx.castExpression());
    } else {
      val op = ctx.getChild(1);
      val ct1 = visitMultiplicativeExpression(ctx.multiplicativeExpression());
      val ct2 = visitCastExpression(ctx.castExpression());

      return commonArithmeticType(ct1, ct2);
    }
  }

  override def visitAdditiveExpression(ctx : CParser.AdditiveExpressionContext) : CType = {
    if (ctx.additiveExpression() == null) {
      return visitMultiplicativeExpression(ctx.multiplicativeExpression());
    } else {
      val op = ctx.getChild(1);
      val ct1 = visitAdditiveExpression(ctx.additiveExpression());
      val ct2 = visitMultiplicativeExpression(ctx.multiplicativeExpression());

      return op.getText() match {
        case "+" => {
          // Addition of pointer with int.
          if (ct1.isInstanceOf[PointerType] && isIntType(ct2)) {
            ct1;
          } else if (ct2.isInstanceOf[PointerType] && isIntType(ct1)) {
            ct2;
          } else {
            // Arithmetic addition.
            // Assume ct1, ct2 are arithmetic.

            commonArithmeticType(ct1, ct2);
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
            PrimitiveType(null, "ptrdiff_t");
          } else if (ct1.isInstanceOf[PointerType] && isIntType(ct2)) {
            ct1;
          } else {
            // Assumes ct1, ct2 are arithmetic

            commonArithmeticType(ct1, ct2);
          }
        }
        case _ => throw new IllegalStateException("Additive operator must be '+' or '-'");
      }
    }
  }

  override def visitShiftExpression(ctx : CParser.ShiftExpressionContext) : CType = {
    if (ctx.shiftExpression() == null) {
      return visitAdditiveExpression(ctx.additiveExpression());
    } else {
      val ct1 = visitShiftExpression(ctx.shiftExpression());
      val ct2 = visitAdditiveExpression(ctx.additiveExpression());

      // Assume ct1, ct2 both of int type.

      // Shift expression result of type of "promoted left operand".
      return commonArithmeticType(ct1, ct2);
    }
  }

  // For relational and equality expression,
  // we assume that the constraints on the types of operands holds.
  // The type of the expression doesn't otherwise depend on the operands.

  override def visitRelationalExpression(ctx : CParser.RelationalExpressionContext) : CType =
    if (ctx.relationalExpression() == null) {
      visitShiftExpression(ctx.shiftExpression());
    } else {
      PrimitiveType(null, "int");
    }

  override def visitEqualityExpression(ctx : CParser.EqualityExpressionContext) : CType =
    if (ctx.equalityExpression() == null) {
      visitRelationalExpression(ctx.relationalExpression());
    } else {
      PrimitiveType(null, "int");
    }

  override def visitAndExpression(ctx : CParser.AndExpressionContext) : CType =
    if (ctx.andExpression() == null) {
      visitEqualityExpression(ctx.equalityExpression());
    } else {
      val ct1 = visitAndExpression(ctx.andExpression());
      val ct2 = visitEqualityExpression(ctx.equalityExpression());
      commonArithmeticType(ct1, ct2);
    }

  override def visitExclusiveOrExpression(ctx : CParser.ExclusiveOrExpressionContext) : CType =
    if (ctx.exclusiveOrExpression() == null) {
      visitAndExpression(ctx.andExpression());
    } else {
      val ct1 = visitExclusiveOrExpression(ctx.exclusiveOrExpression());
      val ct2 = visitAndExpression(ctx.andExpression());
      commonArithmeticType(ct1, ct2);
    }

  override def visitInclusiveOrExpression(ctx : CParser.InclusiveOrExpressionContext) : CType =
    if (ctx.inclusiveOrExpression() == null) {
      visitExclusiveOrExpression(ctx.exclusiveOrExpression());
    } else {
      val ct1 = visitInclusiveOrExpression(ctx.inclusiveOrExpression());
      val ct2 = visitExclusiveOrExpression(ctx.exclusiveOrExpression());
      commonArithmeticType(ct1, ct2);
    }

  override def visitLogicalAndExpression(ctx : CParser.LogicalAndExpressionContext) : CType =
    if (ctx.logicalAndExpression() == null) {
      visitInclusiveOrExpression(ctx.inclusiveOrExpression());
    } else {
      PrimitiveType(null, "int");
    }

  override def visitLogicalOrExpression(ctx : CParser.LogicalOrExpressionContext) : CType =
    if (ctx.logicalOrExpression() == null) {
      visitLogicalAndExpression(ctx.logicalAndExpression());
    } else {
      PrimitiveType(null, "int");
    }

  // Conditional Expression is surprisingly involved.
  override def visitConditionalExpression(ctx : CParser.ConditionalExpressionContext) : CType = {
    if (ctx.conditionalExpression() == null) {
      return visitLogicalOrExpression(ctx.logicalOrExpression());
    } else {
      // _ ? ct1 : ct2;
      val ct1 = visitExpression(ctx.expression());
      val ct2 = visitConditionalExpression(ctx.conditionalExpression());

      return if (isArithmeticType(ct1) && isArithmeticType(ct2)) {
        commonArithmeticType(ct1, ct2);
      } else {
        // Various cases here, but let's assume they're of same/compatible types.
        ct1;
      }
    }
  }

  override def visitAssignmentExpression(ctx : CParser.AssignmentExpressionContext) : CType = {
    if (ctx.assignmentExpression() == null) {
      return visitConditionalExpression(ctx.conditionalExpression());
    } else {
      val op = ctx.getChild(1);
      val ct1 = visit(ctx.unaryExpression());
      val ct2 = visit(ctx.assignmentExpression());

      op.getText() match {
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

      return null;
    }
  }

  override def visitExpression(ctx : CParser.ExpressionContext) : CType =
    visitAssignmentExpression(ctx.assignmentExpression());

  // Sigh. ANTLR visitors, must explicitly visit last rule
  override def visitTypeInferenceFixture(ctx : CParser.TypeInferenceFixtureContext) : CType =
    visitExpression(ctx.expression());
}

object TypeInference {
  def inferType(program : String, of : String) : CType = {
    val in = (if (program != null) program + ";" else "") + of;
    val input = new ANTLRInputStream(in);
    val lexer = new CLexer(input);
    val tokens = new CommonTokenStream(lexer);
    val parser = new CParser(tokens);

    val walker = new ParseTreeWalker();
    val tree = parser.typeInferenceFixture(); // translationUnit + expression

    val defineScopesPhase = new DefineScopesPhase[CType]();
    walker.walk(defineScopesPhase, tree);
    val scopes = defineScopesPhase.scopes;

    val strCons = new StringConstruction(tokens, scopes);
    walker.walk(strCons, tree);

    val tooler = new TypeInference(strCons);
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