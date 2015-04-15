package edu.nus.worksheet.instrumentor

// Go from CType to declaration (string),
// e.g. PrimitiveType("x", "int") -> "int x"
object CTypeToDeclaration {
  // TODO: For external use, it would be more convenient to return
  // as a String (s, d) => s"$s $d", rather than the 2-tuple.
  def declarationOf(t : CType, id : String) : (String, String) =
    t match {
      case pt : PrimitiveType => declarationOf(pt, id);
      case at : ArrayType => declarationOf(at, id);
      case pt : PointerType => declarationOf(pt, id);
      case st : StructType => declarationOf(st, id);
      case et : EnumType => declarationOf(et, id);
      case ft : FunctionType => declarationOf(ft, id);
      case _ => ("Nyah", "");
    }

  def declarationOf(ct : PrimitiveType, id : String) : (String, String) = {
    assert(ct.ctype != null);

    (ct.ctype, id);
  }

  def declarationOf(at : ArrayType, id : String) : (String, String) = {
    val n = if (at.n != null) at.n else "";
    val (s, d) = declarationOf(at.of, s"$id[$n]");

    (s, d)
  }

  def declarationOf(pt : PointerType, id : String) : (String, String) = {
    val (s, d) = declarationOf(pt.of, s"(*$id)");

    (s, d)
  }

  def declarationOf(st : StructType, id : String) : (String, String) = {
    st.structTag match {
      case tag : String => {
        assert(st.id != null);

        (s"${st.structOrUnion} $tag", id);
      }
      case null => {
        assert(st.id != null);

        val members = st.members.map({ ct =>
          declarationOf(ct, id);
        }).mkString(" ");

        (s"${st.structOrUnion} { $members }", id);
      }
    }
  }

  def declarationOf(et : EnumType, id : String) : (String, String) =
    et.enumTag match {
      case tag : String => {
        assert(et.id != null);

        (s"enum $tag", id);
      }
      case null =>
        throw new UnsupportedOperationException("Can't create declaration for anonymous enum.");
    }

  def declarationOf(ft : FunctionType, id : String) : (String, String) = {
    val (rs, rd) = declarationOf(ft.returnType, null);
    val rt = join(rs, rd);

    val params = ft.parameterTypes.map({ ct =>
      join(declarationOf(ct, null));
    }).mkString(" ");

    (rt, s"${id}($params)");
  }

  def join(r : (String, String)) : String = {
    val (s, d) = r;
    s + (if (d != null) " " + d else "");
  }
}