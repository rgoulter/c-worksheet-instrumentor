package edu.nus.worksheet.instrumentor;

// Go from CType to declaration (string),
// e.g. PrimitiveType("x", "int") -> "int x"
object CTypeToDeclaration {
  private[CTypeToDeclaration] def specsDeclrOf(
      ct: CType,
      declr: String
  ): (String, String) =
    ct match {
      case PrimitiveType(_, pt) =>
        (pt, declr);
      case ArrayType(_, _, n, of) => {
        val nStr = n.getOrElse("");
        specsDeclrOf(of, s"$declr[$nStr]");
      }
      case PointerType(_, of) =>
        of match {
          case _: ArrayType => specsDeclrOf(of, s"(*$declr)");
          case _            => specsDeclrOf(of, s"*$declr");
        }
      case StructType(_, sOrU, tag, members) =>
        tag match {
          case Some(tag) =>
            (s"$sOrU $tag", declr);
          case None => {
            // Anonymous
            val memStr = members
              .map({ ct =>
                declarationOf(ct, ct.id.get) + ";";
              })
              .mkString(" ");
            (s"$sOrU { $memStr }", declr);
          }
        }
      case EnumType(_, tag, _) => {
        if tag != null then {
          (s"enum $tag", "");
        } else {
          throw new UnsupportedOperationException(
            "Cannot handle typeName for anonymous enum"
          );
        }
      }
      case FunctionType(_, rtnType, params) => {
        val (rtnS, rtnD) = specsDeclrOf(rtnType, "");
        val rtnDStr = if rtnD != "" then rtnD + " " else "";
        val paramS = params.map(stringOfTypeName(_)).mkString(",");
        (rtnS, s"$rtnDStr($declr)($paramS)");
      }
      case _ =>
        throw new UnsupportedOperationException(
          s"Cannot give string of type $ct"
        );
    }

  def declarationOf(ct: CType, id: String): String = {
    val (s, d) = specsDeclrOf(ct, id);
    s + (if !d.isEmpty() then " " + d; else "");
  }

  def stringOfTypeName(ct: CType): String = {
    val (s, d) = specsDeclrOf(ct, "");
    s + (if !d.isEmpty() then " " + d; else "");
  }

  def join(r: (String, String)): String = {
    val (s, d) = r;
    s + (if d != null then " " + d else "");
  }
}
