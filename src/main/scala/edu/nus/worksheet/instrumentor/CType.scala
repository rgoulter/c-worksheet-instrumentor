package edu.nus.worksheet.instrumentor

import scala.beans.BeanProperty;
import scala.collection.JavaConversions._;
import scala.collection.mutable;

// Making use of CType allows us to pass objects to
// StringTemplates
abstract class CType {
  val template : String;
  val id : String;

  def idOrEmpty() : String = (if (id != null) id else "");

  // Prefix the id with the given nId.
  def fixId(nId : String) : CType =
    this;

  // The id of this CType must begin with the given prefix;
  // return a CType with this Id removed.
  def unfixId(prefix : String) : CType =
    this;
};



case class PrimitiveType(@BeanProperty id : String,
                         @BeanProperty ctype : String)
extends CType {
  @BeanProperty val template = "output_primitive";

  override def fixId(nId : String) : PrimitiveType =
    PrimitiveType(nId + idOrEmpty, ctype);

  override def unfixId(prefix : String) : PrimitiveType = {
    assert(id != null);
    assert(id.startsWith(prefix), s"Expected $id to start with $prefix.");
    PrimitiveType(id.substring(prefix.length()), ctype);
  }
}



case class ArrayType(@BeanProperty id : String,
                     @BeanProperty index : String,
                     @BeanProperty n : String,
                     @BeanProperty of : CType)
extends CType {
  @BeanProperty val template = "output_array";

  override def fixId(nId : String) : ArrayType =
    ArrayType(nId + idOrEmpty, index, n, of.fixId(nId));

  override def unfixId(prefix : String) : ArrayType = {
    assert(id != null);
    assert(id.startsWith(prefix), s"Expected $id to start with $prefix.");
    ArrayType(id.substring(prefix.length()), index, n, of.unfixId(prefix));
  }
}



// `of` may be `null`.
// If non-null, StringConstruction will assume we can output the next type.
case class PointerType(@BeanProperty id : String,
                       @BeanProperty of : CType) extends CType {
  @BeanProperty val template = "output_pointer";

  override def fixId(nId : String) : PointerType =
    PointerType(nId + idOrEmpty, of.fixId(nId));

  override def unfixId(prefix : String) : PointerType = {
    assert(id != null);
    assert(id.startsWith(prefix), s"Expected $id to start wtih $prefix.");
    PointerType(id.substring(prefix.length()), of.unfixId(prefix));
  }
}



case class StructType(@BeanProperty id : String,
                      @BeanProperty structType : String, // e.g. struct MyStruct, MyStruct_t
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
      val memberName = m.unfixId(id).id;
      membersMap += memberName -> m;
    }

    return mapAsJavaMap(membersMap);
  }
  
  @BeanProperty val template = "output_struct";

  // memberId the name of the member,
  // i.e. 'x' of "s.x"
  def getMember(memberId : String) : Option[CType] =
    members.find({ m => m.id == id + "." + memberId });

  override def fixId(nId : String) : StructType =
    StructType(nId + idOrEmpty, structType, members.map(_.fixId(nId)));

  override def unfixId(prefix : String) : StructType = {
    assert(id != null);
    assert(id.startsWith(prefix), s"Expected $id to start with $prefix.");
    StructType(id.substring(prefix.length()), structType, members.map(_.unfixId(prefix)));
  }
}



// Numeric value of constants not important.
case class EnumType(@BeanProperty id : String,
                    @BeanProperty structType : String, // e.g. struct MyStruct, MyStruct_t
                    constants : Seq[String])
extends CType {
  // Seq is easier to deal with.
  def getConstants() : Array[String] = constants.toArray;
  
  @BeanProperty val template = "output_enum";

  override def fixId(nId : String) : EnumType =
    EnumType(nId + idOrEmpty, structType, constants);

  override def unfixId(prefix : String) : EnumType = {
    assert(id != null);
    assert(id.startsWith(prefix), s"Expected $id to start with $prefix.");
    EnumType(id.substring(prefix.length()), structType, constants);
  }
}



case class FunctionType(@BeanProperty id : String,
                        returnType : CType,
                        parameterTypes : Seq[CType]) extends CType {
  // We don't have an ST4 template for outputting Functions.
  // Not sure whether this makes sense or not.
  @BeanProperty val template = "output_function";

  override def fixId(nId : String) : FunctionType =
    FunctionType(nId + idOrEmpty, returnType, parameterTypes);

  override def unfixId(prefix : String) : FunctionType = {
    assert(id != null);
    assert(id.startsWith(prefix), s"Expected $id to start with $prefix.");
    FunctionType(id.substring(prefix.length()), returnType, parameterTypes);
  }
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



private[instrumentor] case class Placeholder() extends CType {
  val id = "placeholder";
  val template = "error";
}