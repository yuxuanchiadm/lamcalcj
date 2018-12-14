package org.lamcalcj.utils

import org.lamcalcj.ast.Lambda._
import org.lamcalcj.utils.Trampoline._

object Utils {
  def alphaConversion(term: Term, usedNames: Set[String] = Set.empty, mapping: Map[Identifier, Identifier] = Map.empty): Term =
    alphaConversionT(term, usedNames, mapping).runT

  def alphaConversionT(term: Term, usedNames: Set[String] = Set.empty, mapping: Map[Identifier, Identifier] = Map.empty): Trampoline[Term] =
    term match {
      case Var(identifier) =>
        Done(mapping.get(identifier).map(Var).getOrElse(term))
      case Abs(variable, term) =>
        if (usedNames.contains(variable.identifier.name)) {
          val unusedName: String = findUnusedName(variable.identifier.name, usedNames)
          val identifier: Identifier = Identifier(unusedName)
          for {
            currentTerm <- alphaConversionT(term, usedNames + unusedName, mapping + (variable.identifier -> identifier))
          } yield Abs(Var(identifier), currentTerm)
        } else for {
          currentTerm <- alphaConversionT(term, usedNames + variable.identifier.name, mapping)
        } yield Abs(variable, currentTerm)
      case App(term, argument) =>
        for {
          currentTerm <- alphaConversionT(term, usedNames, mapping)
          currentArgument <- alphaConversionT(argument, usedNames, mapping)
        } yield App(currentTerm, currentArgument)
    }
  def freeVariables(term: Term, bounds: Set[Identifier] = Set.empty): Set[Identifier] =
    freeVariablesT(term, bounds).runT

  def freeVariablesT(term: Term, bounds: Set[Identifier] = Set.empty): Trampoline[Set[Identifier]] =
    term match {
      case Var(identifier) =>
        if (bounds.contains(identifier)) Done(Set.empty) else Done(Set(identifier))
      case Abs(variable, term) =>
        freeVariablesT(term, bounds + (variable.identifier))
      case App(term, argument) =>
        for {
          termFV <- freeVariablesT(term, bounds)
          argumentFV <- freeVariablesT(argument, bounds)
        } yield termFV ++ argumentFV
    }

  def hasFreeOccurrence(term: Term, id: Identifier): Boolean =
    hasFreeOccurrenceT(term, id).runT

  def hasFreeOccurrenceT(term: Term, id: Identifier): Trampoline[Boolean] =
    term match {
      case Var(identifier) => Done(identifier == id);
      case Abs(variable, term) =>
        for {
          termFO <- hasFreeOccurrenceT(term, id)
        } yield variable.identifier != id && termFO
      case App(term, argument) =>
        for {
          termFO <- hasFreeOccurrenceT(term, id)
          argumentFO <- hasFreeOccurrenceT(argument, id)
        } yield termFO || argumentFO
    }

  def isAlphaEquivalent(term: Term, other: Term, bounds: Map[Identifier, Identifier] = Map.empty): Boolean =
    isAlphaEquivalentT(term, other, bounds).runT

  def isAlphaEquivalentT(term: Term, other: Term, bounds: Map[Identifier, Identifier] = Map.empty): Trampoline[Boolean] =
    term match {
      case Var(identifier) =>
        Done(other.isInstanceOf[Var] && other.asInstanceOf[Var].identifier == bounds.get(identifier).getOrElse(identifier))
      case Abs(variable, term) =>
        for {
          termAE <- isAlphaEquivalentT(term, other.asInstanceOf[Abs].term, bounds + (variable.identifier -> other.asInstanceOf[Abs].variable.identifier))
        } yield other.isInstanceOf[Abs] && termAE
      case App(term, argument) =>
        for {
          termAE <- isAlphaEquivalentT(term, other.asInstanceOf[App].term, bounds)
          argumentAE <- isAlphaEquivalentT(argument, other.asInstanceOf[App].argument, bounds)
        } yield other.isInstanceOf[App] && termAE && argumentAE
    }

  def isTermValid(term: Term): Boolean =
    isTermValidT(term).runT

  def isTermValidT(term: Term): Trampoline[Boolean] =
    for {
      termBV <- boundedVariables(term)
      termPB <- termBV.map(isTermProperlyBounded(term, _)).getOrElse(Done(false))
    } yield termPB

  def cloneTerm(term: Term, mapping: Map[Identifier, Identifier] = Map.empty): Term =
    cloneTermT(term, mapping).runT

  def cloneTermT(term: Term, mapping: Map[Identifier, Identifier] = Map.empty): Trampoline[Term] =
    term match {
      case Var(identifier) =>
        Done(mapping.get(identifier).map(Var).getOrElse(term))
      case Abs(variable, term) => {
        val identifier: Identifier = Identifier(variable.identifier.name)
        for {
          currentTerm <- cloneTermT(term, mapping + (variable.identifier -> identifier))
        } yield Abs(Var(identifier), currentTerm)
      }
      case App(term, argument) =>
        for {
          currentTerm <- cloneTermT(term, mapping)
          currentArgument <- cloneTermT(argument, mapping)
        } yield App(currentTerm, currentArgument)
    }

  private def findUnusedName(name: String, usedNames: Set[String]): String =
    Stream.from(0).map({ name + _ }).filterNot(usedNames.contains).head

  private def boundedVariables(term: Term): Trampoline[Option[Set[Identifier]]] =
    term match {
      case Var(identifier) =>
        Done(Option(Set.empty))
      case Abs(variable, term) =>
        for {
          termBV <- boundedVariables(term)
        } yield for {
          termBounds <- termBV
          result <- if (termBounds.contains(variable.identifier)) Option.empty else Option(termBounds + variable.identifier)
        } yield result
      case App(term, argument) =>
        for {
          termBV <- boundedVariables(term)
          argumentBV <- boundedVariables(argument)
        } yield for {
          termBounds <- termBV
          argumentBounds <- argumentBV
          result <- if (termBounds.exists(argumentBounds.contains)) Option.empty else Option(termBounds ++ argumentBounds)
        } yield result
    }

  private def isTermProperlyBounded(term: Term, unbounded: Set[Identifier]): Trampoline[Boolean] =
    term match {
      case Var(identifier) =>
        Done(!unbounded.contains(identifier))
      case Abs(variable, term) =>
        isTermProperlyBounded(term, unbounded - variable.identifier)
      case App(term, argument) =>
        for {
          termPB <- isTermProperlyBounded(term, unbounded)
          argumentPB <- isTermProperlyBounded(argument, unbounded)
        } yield termPB && argumentPB
    }
}