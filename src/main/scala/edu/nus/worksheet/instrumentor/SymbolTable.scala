// Adapting from PL Impl Patterns
package edu.nus.worksheet.instrumentor

import scala.collection.mutable.Map
import scala.collection.mutable.LinkedHashMap


trait Scope {
  var enclosingScope : Option[Scope];

  // Scopes in C have variables, as well as structs/unions, enums, typedefs.
  val symbols = Map[String, CType]();
  val declaredStructs = Map[String, StructType]();
  val declaredEnums = Map[String, EnumType]();
  val declaredTypedefs = Map[String, CType]();

  val scopeName : String;

  private def resolve[T](mapFor : Scope => Map[String, T], name : String) : Option[T] =
    mapFor(this).get(name) match {
      case Some(s) => Some(s);
      case None => {
        // Not in this scope; check enclosing scope.
        enclosingScope match {
          case Some(scope) => scope.resolve(mapFor, name);
          case None => None;
        }
      }
    }

  private def define[T](map : Map[String, T], name : String, sym : T) =
    map += name -> sym;

  def defineSymbol(varCt : CType) =
    define[CType](symbols, varCt.id, varCt);

  def resolveSymbol(id : String) : Option[CType] =
    resolve[CType](_.symbols, id);

  def defineStruct(strCt : StructType) =
    define[StructType](declaredStructs, strCt.structTag, strCt);

  def resolveStruct(id : String) : Option[StructType] =
    resolve[StructType](_.declaredStructs, id);

  def defineEnum(enumCt : EnumType) =
    define[EnumType](declaredEnums, enumCt.enumTag, enumCt);

  def resolveEnum(id : String) : Option[EnumType] =
    resolve[EnumType](_.declaredEnums, id);

  def defineTypedef(id : String, tdCt : CType) =
    define[CType](declaredTypedefs, id, tdCt);

  def resolveTypedef(id : String) : Option[CType] =
    resolve[CType](_.declaredTypedefs, id);

  override def toString() : String =
    scopeName + ":" + symbols.keySet.toString();
}


class GlobalScope(var enclosingScope : Option[Scope] = None) extends Scope {
  val scopeName = "globals";
}


class BlockScope(var enclosingScope : Option[Scope], val scopeName : String = "local") extends Scope;


class FunctionScope(var enclosingScope : Option[Scope], val scopeName : String = "local") extends Scope;