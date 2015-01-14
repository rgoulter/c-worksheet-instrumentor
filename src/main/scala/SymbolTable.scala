// Adapting from PL Impl Patterns

import scala.collection.mutable.Map
import scala.collection.mutable.LinkedHashMap


// was an Interface in Java; Java doesn't have multi inherit.
// BaseScope is needed e.g. for Global, Local.
// but Scope is Iface b/c we have FunctionSymbol which
// is-a symbol but also is-a Scope.
// (which Java can't handle).
trait Scope {
  var enclosingScope : Option[Scope];
  val symbols = new LinkedHashMap[String, Symbol]();

  def getScopeName() : String;

  def resolve(name : String) : Option[Symbol] =
    symbols.get(name) match {
      case Some(s) => Some(s);
      case None => {
        // Not in this scope; check enclosing scope.
        enclosingScope match {
          case Some(scope) => scope.resolve(name);
          case None => None;
        }
      }
    }

  def define(sym : Symbol) = {
    symbols.put(sym.name, sym);
    sym.scope = Some(this);
  }

  override def toString() : String =
    getScopeName() + ":" + symbols.keySet.toString();
}


class GlobalScope(var enclosingScope : Option[Scope]) extends Scope {
  def getScopeName() : String = "globals";
}


class LocalScope(var enclosingScope : Option[Scope]) extends Scope {
  def getScopeName() : String = "locals";
}

// TODO: Symbol Type.
// (Case Class?).

class Symbol(val name : String, val symType : String = "INVALID") {
  // what to do with Type = enum { tINVALID, tVOID, tINT, tFLOAT } ?
  // e.g. use Scala equiv. of Enum, or...

  // Because we don't start with a scope,
  // we need to have Scope be an Option here.
  var scope : Option[Scope] = None;

  override def toString() : String = 
    symType match {
      case "INVALID" => name;
      case _ => "<" + name + ":" + symType + ">";
    }
}


class VariableSymbol(name : String, symType : String) extends Symbol(name, symType);


class FunctionSymbol(override val name : String, val retType : String, var enclosingScope : Option[Scope])
  extends Symbol(name, retType)
  with Scope {

  def getScopeName() : String = name;

  override def toString() : String =
    // where "arguments" is the LinkedHashMap (of Scope?)
    "function" + super.toString() + ":" + this.symbols.values;
}
