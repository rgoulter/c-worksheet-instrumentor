// Adapting from PL Impl Patterns
package edu.nus.worksheet.instrumentor

import scala.collection.mutable.Map
import scala.collection.mutable.LinkedHashMap


trait Scope[T] {
  var enclosingScope : Option[Scope[T]];
  val symbols = new LinkedHashMap[String, T]();

  val scopeName : String;

  def resolve(name : String) : Option[T] =
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

  def define(name : String, sym : T) = {
    symbols.put(name, sym);
  }

  override def toString() : String =
    scopeName + ":" + symbols.keySet.toString();
}


class GlobalScope[T](var enclosingScope : Option[Scope[T]] = None) extends Scope[T] {
  val scopeName = "globals";
}


class BlockScope[T](var enclosingScope : Option[Scope[T]], val scopeName : String = "local") extends Scope[T];


class FunctionScope[T](var enclosingScope : Option[Scope[T]], val scopeName : String = "local") extends Scope[T];