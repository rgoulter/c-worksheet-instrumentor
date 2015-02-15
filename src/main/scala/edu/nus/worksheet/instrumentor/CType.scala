package edu.nus.worksheet.instrumentor

import scala.beans.BeanProperty

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
  // Seq is easier to deal with.
  def getMembers() : Array[CType] = members.toArray;
  
  @BeanProperty val template = "output_struct";
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