package org.lamcalcj.utils

import org.lamcalcj.ast.Lambda._
import scala.annotation.tailrec

object Utils {
  def alphaConversion[T <: Term](term: T, usedNames: Set[String] = Set.empty, mapping: Map[Identifier, Identifier] = Map.empty): Term =
    term match {
      case Var(identifier) =>
        mapping.get(identifier).map(Var).getOrElse(term)
      case Abs(variable, term) =>
        if (usedNames.contains(variable.identifier.name)) {
          val unusedName: String = findUnusedName(variable.identifier.name, usedNames)
          val identifier: Identifier = Identifier(unusedName)
          Abs(Var(identifier), alphaConversion(term, usedNames + unusedName, mapping + (variable.identifier -> identifier)))
        } else
          Abs(variable, alphaConversion(term, usedNames + variable.identifier.name, mapping))
      case App(term, argument) =>
        App(alphaConversion(term, usedNames, mapping), alphaConversion(argument, usedNames, mapping))
    }

  private def findUnusedName(name: String, usedNames: Set[String]): String =
    Stream.from(0).map({ name + _ }).filterNot(usedNames.contains).head
}