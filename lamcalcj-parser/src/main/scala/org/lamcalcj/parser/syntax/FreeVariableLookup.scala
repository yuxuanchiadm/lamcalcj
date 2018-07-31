package org.lamcalcj.parser.syntax

import org.lamcalcj.ast.Lambda.Identifier

object FreeVariableLookup {
  def empty: FreeVariableLookup = new MapFreeVariableLookup(Map.empty)

  private final class MapFreeVariableLookup(map: Map[String, Identifier]) extends FreeVariableLookup {
    override def +(kv: (String, Identifier)): FreeVariableLookup = new MapFreeVariableLookup(map + kv)

    override def apply(name: String): Identifier = map(name)

    override def toMap(): Map[String, Identifier] = map
  }
}

trait FreeVariableLookup {
  def +(kv: (String, Identifier)): FreeVariableLookup

  def apply(name: String): Identifier

  def toMap(): Map[String, Identifier]
}
