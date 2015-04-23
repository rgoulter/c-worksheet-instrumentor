package edu.nus.worksheet.instrumentor

import java.util.regex.Pattern;
import scala.beans.BeanProperty;
import scala.collection.JavaConversions._;
import scala.collection.mutable;

// Making use of CType allows us to pass objects to
// StringTemplates
abstract class CType {
  val template : String;
  val id : String;

  def idOrEmpty() : String = (if (id != null) id else "");

  // Operate on the identifier of this CType
  def fId(f : String => String) : CType =
    this;

  def changeId(nId : String) =
    fId({ _ => nId});
};



case class PrimitiveType(@BeanProperty id : String,
                         @BeanProperty ctype : String)
extends CType {
  @BeanProperty val template = "output_primitive";

  override def fId(f : String => String) : PrimitiveType =
    PrimitiveType(f(idOrEmpty), ctype);
}



case class ArrayType(@BeanProperty id : String,
                     @BeanProperty index : String,
                     @BeanProperty n : String,
                     @BeanProperty of : CType)
extends CType {
  @BeanProperty val template = "output_array";

  private[ArrayType] val OfIdRegex = "(.*)\\[(.*)\\]".r;

  // Get the `i` of `a[i]` in the identifier of `of` CType.
  def getSubscript() : String = {
    assert(of != null);

    of.id match {
      case OfIdRegex(_, subscr) => subscr;
      case _ =>
        throw new IllegalStateException("id of array's `of` must be of form 'id[subscript]");
    }
  }

  // Coerce Arr(a, Prim(a[i], T)) => Pointer(a, Prim((*a + i), T))
  def coerceToPointerType() : PointerType = {
    val ptrOf = of.fId({ ofId =>
      // a[i] => (*a + i)
      ofId match {
        case OfIdRegex(_, offset) => {
          if (offset != "" && offset != "0") {
            s"(*$id + ($offset))";
          } else {
            s"(*$id)";
          }
        }
        case _ =>
          throw new IllegalStateException("id of array's `of` must be of form 'id[subscript]");
      }
    });

    PointerType(id, ptrOf);
  }

  override def fId(f : String => String) : ArrayType = {
    // CType which StringConstruction forms for int x[4] is like
    //   Arr(x, Prim(x[x_0]))
    // So, we need to keep the last [..] in the `of` CType
    def fOf(ofId : String) : String = {
      if (ofId != null && ofId.length > 0) {
        val idx = ofId.lastIndexOf('[');
        val (arrId, subscript) = (ofId.substring(0, idx), ofId.substring(idx));
        f(arrId) + subscript;
      } else {
        // If id is null, the "type of" doesn't matter as much.
        // e.g. type inference can replace the index if need be.
        assert(idOrEmpty == "");
        f(idOrEmpty) + "[]"
      }
    }

    ArrayType(f(idOrEmpty), index, n, of.fId(fOf));
  }
}



// `of` may be `null`.
// If non-null, StringConstruction will assume we can output the next type.
case class PointerType(@BeanProperty id : String,
                       @BeanProperty of : CType) extends CType {
  @BeanProperty val template = "output_pointer";

  override def fId(f : String => String) : PointerType = {
    // StringConstruction gives us the CType like
    //   PointerType(p, Prim((*p)))
    // (Though, by right, not necessarily having the parentheses.
    // ... but we can just make the Id this, anyway.
    def fOf(ofId : String) : String = {
      "(*" + f(id) + ")";
    }

    PointerType(f(idOrEmpty), of.fId(fOf));
  }
}



case class StructType(@BeanProperty id : String,
                      structOrUnion : String,
                      @BeanProperty structTag : String, // e.g. struct MyStruct, MyStruct_t
                      members : Seq[CType])
extends CType {
  // `getMembers()` is used in ST4 constructs.stg, where we want it to be a map
  // from the struct's member name, to the CType of that member.
  // We used LinkedHashMap to ensure a consistent iteration order.
  // (i.e. in the order they were inserted).
  def getMembers() : java.util.Map[String, CType] = {
    val membersMap = new mutable.LinkedHashMap[String, CType]();

    val structIdLen = id.length();
    for (m <- members) {
      val memberName = m.fId({ mId => mId.substring(mId.lastIndexOf('.')) }).id;
      membersMap += memberName -> m;
    }

    return mapAsJavaMap(membersMap);
  }

  @BeanProperty val template = "output_struct";

  // memberId the name of the member,
  // i.e. 'x' of "s.x"
  def getMember(memberId : String) : Option[CType] =
    members.find({ m => m.id == id + "." + memberId });

  override def fId(f : String => String) : StructType = {
    // CType which StringConstruction forms for Struct {int x} s; is like
    //   Struct(s, Seq(Prim(s.x)))
    // So, we need to keep the last id in the members CType
    def fMember(memberId : String) : String = {
      if (id != null && id != "") {
        val idx = Math.max(memberId.indexOf('.'), 0);
        val (strId, memberName) = (memberId.substring(0, idx), memberId.substring(idx));
        f(strId) + memberName;
      } else {
        f("") + "." + memberId;
      }
    }

    StructType(f(idOrEmpty), structOrUnion, structTag, members.map(_.fId(fMember)));
  }
}



// Numeric value of constants not important.
case class EnumType(@BeanProperty id : String,
                    @BeanProperty enumTag : String, // e.g. struct MyStruct, MyStruct_t
                    constants : Seq[String])
extends CType {
  // Seq is easier to deal with.
  def getConstants() : Array[String] = constants.toArray;

  @BeanProperty val template = "output_enum";

  override def fId(f : String => String) : EnumType =
    EnumType(f(idOrEmpty), enumTag, constants);
}



case class FunctionType(@BeanProperty id : String,
                        returnType : CType,
                        parameterTypes : Seq[CType]) extends CType {
  // We don't have an ST4 template for outputting Functions.
  // Not sure whether this makes sense or not.
  @BeanProperty val template = "output_function";

  override def fId(f : String => String) : FunctionType =
    FunctionType(f(idOrEmpty), returnType, parameterTypes);
}



case class VarArgType() extends CType {
  // VarArg as a type, for function type parameter.
  // This is a special case, doesn't need to be printed.
  val id = "va_list";
  val template = "error";
}



case class VoidType() extends CType {
  val id = "void";
  val template = "error";
}



// ForwardDeclarationType acts as a place-holder CType for
// forward-declared structs/unions.
//
// Declarations are only for types in the same scope.
case class ForwardDeclarationType(id : String, tag : String, scope : Scope) extends CType {
  val template = "error";

  def getDeclaredCType() : Option[CType] =
    scope.resolveStruct(tag) match {
      case st : Option[StructType] => st;
      case _ => None
    }

  def hasTypeBeenDeclared() : Boolean =
    getDeclaredCType() match {
      case Some(ct) => true;
      case None => false;
    }
}



private[instrumentor] case class Placeholder() extends CType {
  val id = "placeholder";
  val template = "error";
}