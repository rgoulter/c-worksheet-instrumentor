package edu.nus.worksheet.instrumentor;

import java.util.regex.Pattern;

import scala.beans.BeanProperty;
import scala.collection.mutable;
import scala.jdk.CollectionConverters.*;

// Making use of CType allows us to pass objects to
// StringTemplates
abstract class CType {
  val template: String;
  val id: Option[String];

  def getId(): String =
    id.getOrElse("");

  // Operate on the identifier of this CType
  def fId(f: String => String): CType =
    this;

  def changeId(nId: String) =
    fId({ _ => nId });
};

case class PrimitiveType(id: Option[String], @BeanProperty ctype: String)
    extends CType {
  def this(id: String, ctype: String) =
    this(Option(id), ctype);

  @BeanProperty val template = "output_primitive";

  override def fId(f: String => String): PrimitiveType =
    PrimitiveType(Some(f(getId())), ctype);
}

case class ArrayType(
    id: Option[String],
    index: Option[String],
    n: Option[String],
    @BeanProperty of: CType
) extends CType {
  def this(id: String, idx: String, n: String, of: CType) =
    this(Option(id), Option(idx), Option(n), of);

  def getIndex(): String =
    index.getOrElse(null);

  def getN(): String =
    n.getOrElse(null);

  @BeanProperty val template = "output_array";

  private[ArrayType] val OfIdRegex = "(.*)\\[(.*)\\]".r;

  // Get the `i` of `a[i]` in the identifier of `of` CType.
  def getSubscript(): String = {
    assert(of != null);

    of.id match {
      case Some(OfIdRegex(_, subscr)) => subscr;
      case _ =>
        throw new IllegalStateException(
          "id of array's `of` must be of form 'id[subscript]"
        );
    }
  }

  // Coerce Arr(a, Prim(a[i], T)) => Pointer(a, Prim((*a + i), T))
  def coerceToPointerType(): PointerType = {
    val ptrOf = of.fId({ ofId =>
      // a[i] => (*a + i)
      ofId match {
        case OfIdRegex(_, offset) => {
          if offset != "" && offset != "0" then {
            s"(*${getId()} + ($offset))";
          } else {
            s"(*${getId()})";
          }
        }
        case _ =>
          throw new IllegalStateException(
            "id of array's `of` must be of form 'id[subscript]"
          );
      }
    });

    PointerType(id, ptrOf);
  }

  override def fId(f: String => String): ArrayType = {
    // CType which StringConstruction forms for int x[4] is like
    //   Arr(x, Prim(x[x_0]))
    // So, we need to keep the last [..] in the `of` CType
    def fOf(ofId: String): String = {
      if ofId != null && ofId.length > 0 then {
        val idx = ofId.lastIndexOf('[');
        val (arrId, subscript) = (ofId.substring(0, idx), ofId.substring(idx));
        f(arrId) + subscript;
      } else {
        // If id is null, the "type of" doesn't matter as much.
        // e.g. type inference can replace the index if need be.
        assert(getId() == "");
        f(getId()) + "[]";
      }
    }

    ArrayType(Some(f(getId())), index, n, of.fId(fOf));
  }
}

// `of` may be `null`.
// If non-null, StringConstruction will assume we can output the next type.
case class PointerType(id: Option[String], of: CType) extends CType {
  def this(id: String, of: CType) =
    this(Option(id), of);

  // ST4 is unable to recognise `getTemplate` from an anonymous class extending CType.
  class FuncPtrType(val id: Option[String]) extends CType {
    val template = "output_function";
    def getTemplate(): String = template;
  }

  def getOf(): CType =
    of match {
      // ST4 won't render a null `of`, and we don't want pointer-of-pointer rendered.
      case p: PointerType => null;
      case FunctionType(f, _, _) => {

        // `output_function` assumes `id` has value of address of a function,
        // so we keep the pointer's ID.
        new FuncPtrType(id);
      }
      case t => t;
    }

  @BeanProperty val template = "output_pointer";

  override def fId(f: String => String): PointerType = {
    // StringConstruction gives us the CType like
    //   PointerType(p, Prim((*p)))
    // (Though, by right, not necessarily having the parentheses.
    // ... but we can just make the Id this, anyway.
    def fOf(ofId: String): String = {
      "(*" + f(getId()) + ")";
    }

    PointerType(Some(f(getId())), of.fId(fOf));
  }
}

case class StructType(
    id: Option[String],
    structOrUnion: String,
    structTag: Option[String],
    members: Seq[CType]
) extends CType {
  def this(id: String, sOrU: String, tag: String, members: Seq[CType]) =
    this(Option(id), sOrU, Option(tag), members);

  def getStructTag(): String =
    // Mostly for ST4's constructs.stg benefit, but we use null
    // rather than empty string to indicate "no tag".
    structTag.getOrElse(null);

  // `getMembers()` is used in ST4 constructs.stg, where we want it to be a map
  // from the struct's member name, to the CType of that member.
  // We used LinkedHashMap to ensure a consistent iteration order.
  // (i.e. in the order they were inserted).
  def getMembers(): java.util.Map[String, CType] = {
    val membersMap = new mutable.LinkedHashMap[String, CType]();

    val structIdLen = getId().length();
    for m <- members do {
      val memberName = m
        .fId({ mId => mId.substring(mId.lastIndexOf('.')) })
        .id
        .getOrElse(throw new IllegalStateException("Member must have id."));
      membersMap += memberName -> m;
    }

    return membersMap.asJava;
  }

  @BeanProperty val template = "output_struct";

  // memberId the name of the member,
  // i.e. 'x' of "s.x"
  def getMember(memberId: String): Option[CType] =
    members.find({ m => m.id == Some(getId() + "." + memberId) });

  override def fId(f: String => String): StructType = {
    // CType which StringConstruction forms for Struct {int x} s; is like
    //   Struct(s, Seq(Prim(s.x)))
    // So, we need to keep the last id in the members CType
    def fMember(memberId: String): String =
      id match {
        case Some(id) => {
          val idx = Math.max(memberId.indexOf('.'), 0);
          val (strId, memberName) =
            (memberId.substring(0, idx), memberId.substring(idx));
          f(strId) + memberName;
        }
        case None =>
          f("") + "." + memberId;
      }

    StructType(
      Some(f(getId())),
      structOrUnion,
      structTag,
      members.map(_.fId(fMember))
    );
  }
}

// Numeric value of constants not important.
case class EnumType(
    id: Option[String],
    enumTag: Option[String], // e.g. struct MyStruct, MyStruct_t
    constants: Seq[String]
) extends CType {
  def this(id: String, tag: String, constants: Seq[String]) =
    this(Option(id), Option(tag), constants);

  def getEnumTag(): String =
    enumTag.getOrElse(null);

  // Seq is easier to deal with.
  def getConstants(): Array[String] = constants.toArray;

  @BeanProperty val template = "output_enum";

  override def fId(f: String => String): EnumType =
    EnumType(Some(f(getId())), enumTag, constants);
}

case class FunctionType(
    id: Option[String],
    returnType: CType,
    parameterTypes: Seq[CType]
) extends CType {
  def this(id: String, rtn: CType, params: Seq[CType]) =
    this(Option(id), rtn, params);

  // We don't have an ST4 template for outputting Functions.
  // Not sure whether this makes sense or not.
  @BeanProperty val template = "output_function";

  override def fId(f: String => String): FunctionType =
    FunctionType(Some(f(getId())), returnType, parameterTypes);
}

case class VarArgType() extends CType {
  // VarArg as a type, for function type parameter.
  // This is a special case, doesn't need to be printed.
  val id = None;
  val template = "error";
}

case class VoidType() extends CType {
  val id = None;
  val template = "error";
}

// ForwardDeclarationType acts as a place-holder CType for
// forward-declared structs/unions.
//
// Declarations are only for types in the same scope.
case class ForwardDeclarationType(id: Option[String], tag: String)
    extends CType {
  val template = "error";

  def getDeclaredCType(scope: Scope): Option[CType] =
    scope.resolveStruct(tag)

  def hasTypeBeenDeclared(scope: Scope): Boolean =
    getDeclaredCType(scope) match {
      case Some(ct) => true;
      case None     => false;
    }
}

private[instrumentor] case class Placeholder() extends CType {
  val id = Some("placeholder");
  val template = "error";
}
