package edu.nus.worksheet.instrumentor

import scala.beans.BeanProperty;
import scala.collection.JavaConversions._;
import scala.collection.mutable;

// Making use of CType allows us to pass objects to
// StringTemplates
abstract class CType {
  val template : String;
  val id : String;
};

case class PrimitiveType(@BeanProperty id : String,
                         @BeanProperty ctype : String)
extends CType {
  @BeanProperty val template = "output_primitive";
}

case class ArrayType(@BeanProperty id : String,
                     @BeanProperty index : String,
                     @BeanProperty n : String,
                     @BeanProperty of : CType)
extends CType {
  @BeanProperty val template = "output_array";
}

// `of` may be `null`.
// If non-null, StringConstruction will assume we can output the next type.
case class PointerType(@BeanProperty id : String,
                       @BeanProperty of : CType) extends CType {
  @BeanProperty val template = "output_pointer";
}

private[instrumentor] case class Placeholder() extends CType {
  val id = "placeholder";
  val template = "error";
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
      // The id of each member is prefixed by "structId.", so we remove this.
      val memberName = m.id.substring(structIdLen + 1);

      membersMap += memberName -> m;
    }

    return mapAsJavaMap(membersMap);
  }
  
  @BeanProperty val template = "output_struct";

  // memberId the name of the member,
  // i.e. 'x' of "s.x"
  def getMember(memberId : String) : Option[CType] =
    members.find({ m => m.id == id + "." + memberId });
}

// Numeric value of constants not important.
case class EnumType(@BeanProperty id : String,
                    @BeanProperty structType : String, // e.g. struct MyStruct, MyStruct_t
                    constants : Seq[String])
extends CType {
  // Seq is easier to deal with.
  def getConstants() : Array[String] = constants.toArray;
  
  @BeanProperty val template = "output_enum";
}