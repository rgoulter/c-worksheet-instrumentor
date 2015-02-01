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

// With Pointers, since we can't guarantee that they will point-to
// something, it's probably not worth trying.
case class PointerType(@BeanProperty id : String) extends CType {
  @BeanProperty val template = "output_pointer";
}