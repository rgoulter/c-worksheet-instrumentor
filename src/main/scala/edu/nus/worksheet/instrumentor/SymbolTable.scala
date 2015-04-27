// Adapting from PL Impl Patterns
package edu.nus.worksheet.instrumentor

import scala.collection.mutable.Map
import scala.collection.mutable.LinkedHashMap
import edu.nus.worksheet.instrumentor.StringConstruction.fixCType;


trait Scope {
  var enclosingScope : Option[Scope];

  // Scopes in C have variables, as well as structs/unions, enums, typedefs.
  val symbols = LinkedHashMap[String, CType]();
  val declaredStructs = LinkedHashMap[String, StructType]();
  val declaredEnums = LinkedHashMap[String, EnumType]();
  val declaredTypedefs = LinkedHashMap[String, CType]();

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

  // "Flatten" i.e. flatten out any forward declarations.
 def flattenCType(ct : CType) : CType =
    ct match {
      case fd : ForwardDeclarationType =>
        fd.getDeclaredCType(this) match {
          case Some(ct) => {
            assert(!ct.isInstanceOf[ForwardDeclarationType]);
            fixCType(ct, fd.id);
          }
          case None =>
            // For structs w/ extern linkage, they won't be defined in the same file.
            // So we have to allow for cases like that.
            // ASSUME extern declaration (for the given `fd`).
            // Not clear what the best type to resolve it to is. (Remove from dict?).
            fd;
        }
      case ArrayType(id, n, idx, of) =>
        ArrayType(id, n, idx, flattenCType(of));
      case PointerType(id, of) =>
        PointerType(id, flattenCType(of));
      case StructType(id, sOrU, tag, members) => {
        val flatMem = members.map(flattenCType _);
        StructType(id, sOrU, tag, flatMem);
      }
      case FunctionType(id, rt, params) =>
        FunctionType(id, flattenCType(rt), params.map(flattenCType _));
      case x => x;
    }

  // Structs/Unions can be forward-declared.
  // We deal with this using a ForwardDeclaration type.
  // This needs to be flattened.
  def flattenForwardDeclarations() {
    for ((k,v) <- symbols) {
      symbols += k -> flattenCType(v);
    }
    for ((k,v) <- declaredTypedefs) {
      declaredTypedefs += k -> flattenCType(v);
    }
    for ((k,v) <- declaredStructs) {
      declaredStructs += k -> flattenCType(v).asInstanceOf[StructType];
    }
  }

  override def toString() : String =
    scopeName + ":" + symbols.keySet.toString();
}



class Builtins extends Scope {
  var enclosingScope : Option[Scope] = None;
  val scopeName = "builtins";

  // Not sure how this is to be used, but need this to understand
  // stdarg.h, included by stdio.h
  defineTypedef("__builtin_va_list", PrimitiveType(null, "__builtin_va_list "));
}


class GlobalScope(var enclosingScope : Option[Scope] = Some(new Builtins())) extends Scope {
  val scopeName = "globals";
}


class BlockScope(var enclosingScope : Option[Scope], val scopeName : String = "local") extends Scope;


class FunctionScope(var enclosingScope : Option[Scope], val scopeName : String = "local") extends Scope;