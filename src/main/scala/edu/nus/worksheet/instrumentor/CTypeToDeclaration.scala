package edu.nus.worksheet.instrumentor

// Go from CType to declaration (string),
// e.g. PrimitiveType("x", "int") -> "int x"
object CTypeToDeclaration {
  private[CTypeToDeclaration] def specsDeclrOf(ct : CType, declr : String) : (String, String) =
    ct match {
      case PrimitiveType(_, pt) =>
        (pt, declr);
      case ArrayType(_, _, n, of) => {
        val nStr = if (n != null) n else "";
        specsDeclrOf(of, s"$declr[$nStr]");
      }
      case PointerType(_, of) =>
        of match {
          case _ : ArrayType => specsDeclrOf(of, s"(*$declr)");
          case _ => specsDeclrOf(of, s"*$declr");
        }
      case StructType(_, sOrU, tag, members) => {
        if (tag != null) {
          (s"$sOrU $tag", "");
        } else {
          // Anonymous
          val memStr = members.map({ ct =>
            declarationOf(ct, ct.id.get) + ";";
          }).mkString(" ");
          (s"$sOrU { $memStr }", "")
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
        val (rtnS, rtnD) = specsDeclrOf(rtnType, "");
        val rtnDStr = if (rtnD != "") rtnD + " " else "";
        val paramS = params.map(stringOfTypeName(_)).mkString(",");
        (rtnS, s"$rtnDStr($declr)($paramS)");
      }
      case _ => throw new UnsupportedOperationException(s"Cannot give string of type $ct")
    }

  def declarationOf(ct : CType, id : String) : String = {
    val (s, d) = specsDeclrOf(ct, id);
    s + (if (!d.isEmpty()) " " + d; else "");
  }

  def stringOfTypeName(ct : CType) : String = {
    val (s, d) = specsDeclrOf(ct, "");
    s + (if (!d.isEmpty()) " " + d; else "");
  }

  def join(r : (String, String)) : String = {
    val (s, d) = r;
    s + (if (d != null) " " + d else "");
  }
}