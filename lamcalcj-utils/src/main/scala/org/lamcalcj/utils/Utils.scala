package org.lamcalcj.utils

import org.lamcalcj.ast.Lambda._
import scala.annotation.tailrec

object Utils {
  def alphaConversion(term: Term, usedNames: Set[String] = Set.empty, mapping: Map[Identifier, Identifier] = Map.empty): Term =
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

  def freeVariables(term: Term, bounded: Set[Identifier] = Set.empty): Set[Identifier] =
    term match {
      case Var(identifier) =>
        if (bounded.contains(identifier)) Set.empty else Set(identifier)
      case Abs(variable, term) =>
        freeVariables(term, bounded + (variable.identifier))
      case App(term, argument) =>
        freeVariables(term, bounded) ++ freeVariables(argument, bounded)
    }

  def isAlphaEquivalent(term: Term, other: Term, bounds: Map[Identifier, Identifier] = Map.empty): Boolean =
    term match {
      case Var(identifier) =>
        other.isInstanceOf[Var] && other.asInstanceOf[Var].identifier == bounds.get(identifier).getOrElse(identifier)
      case Abs(variable, term) =>
        other.isInstanceOf[Abs] &&
        isAlphaEquivalent(term, other.asInstanceOf[Abs].term, bounds + (variable.identifier -> other.asInstanceOf[Abs].variable.identifier))
      case App(term, argument) =>
        other.isInstanceOf[App] &&
          isAlphaEquivalent(term, other.asInstanceOf[App].term, bounds) &&
          isAlphaEquivalent(argument, other.asInstanceOf[App].argument, bounds)
    }

  private def findUnusedName(name: String, usedNames: Set[String]): String =
    Stream.from(0).map({ name + _ }).filterNot(usedNames.contains).head
}