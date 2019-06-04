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
      case Abs(binding, term) =>
        if (usedNames.contains(binding.name)) {
          val unusedName: String = findUnusedName(binding.name, usedNames)
          val identifier: Identifier = binding.cloneIdentifier(unusedName)
          for {
            currentTerm <- More(() => alphaConversionT(term, usedNames + unusedName, mapping + (binding -> identifier)))
          } yield Abs(identifier, currentTerm)
        } else for {
          currentTerm <- More(() => alphaConversionT(term, usedNames + binding.name, mapping))
        } yield Abs(binding, currentTerm)
      case App(term, argument) =>
        for {
          currentTerm <- More(() => alphaConversionT(term, usedNames, mapping))
          currentArgument <- More(() => alphaConversionT(argument, usedNames, mapping))
        } yield App(currentTerm, currentArgument)
    }

  def freeVariables(term: Term, bounds: Set[Identifier] = Set.empty): Set[Identifier] =
    freeVariablesT(term, bounds).runT

  def freeVariablesT(term: Term, bounds: Set[Identifier] = Set.empty): Trampoline[Set[Identifier]] =
    term match {
      case Var(identifier) =>
        if (bounds.contains(identifier)) Done(Set.empty) else Done(Set(identifier))
      case Abs(binding, term) =>
        More(() => freeVariablesT(term, bounds + binding))
      case App(term, argument) =>
        for {
          termFV <- More(() => freeVariablesT(term, bounds))
          argumentFV <- More(() => freeVariablesT(argument, bounds))
        } yield termFV ++ argumentFV
    }

  def isClosedTerm(term: Term, bounds: Set[Identifier] = Set.empty): Boolean =
    isClosedTermT(term, bounds).runT

  def isClosedTermT(term: Term, bounds: Set[Identifier] = Set.empty): Trampoline[Boolean] =
    term match {
      case Var(identifier) =>
        Done(bounds.contains(identifier))
      case Abs(binding, term) =>
        More(() => isClosedTermT(term, bounds + binding))
      case App(term, argument) =>
        for {
          termClosed <- More(() => isClosedTermT(term, bounds))
          argumentClosed <- More(() => isClosedTermT(argument, bounds))
        } yield termClosed && argumentClosed
    }

  def hasFreeOccurrence(term: Term, id: Identifier): Boolean =
    hasFreeOccurrenceT(term, id).runT

  def hasFreeOccurrenceT(term: Term, id: Identifier): Trampoline[Boolean] =
    term match {
      case Var(identifier) => Done(identifier == id);
      case Abs(binding, term) =>
        for {
          termFO <- More(() => hasFreeOccurrenceT(term, id))
        } yield binding != id && termFO
      case App(term, argument) =>
        for {
          termFO <- More(() => hasFreeOccurrenceT(term, id))
          argumentFO <- More(() => hasFreeOccurrenceT(argument, id))
        } yield termFO || argumentFO
    }

  def isAlphaEquivalent(term: Term, other: Term, bounds: Map[Identifier, Identifier] = Map.empty): Boolean =
    isAlphaEquivalentT(term, other, bounds).runT

  def isAlphaEquivalentT(term: Term, other: Term, bounds: Map[Identifier, Identifier] = Map.empty): Trampoline[Boolean] =
    term match {
      case Var(identifier) =>
        other match {
          case Var(otherIdentifier) => Done(otherIdentifier == bounds.getOrElse(identifier, identifier))
          case term => Done(false)
        }
      case Abs(binding, term) =>
        other match {
          case Abs(otherBinding, otherTerm) =>
            for {
              termAE <- More(() => isAlphaEquivalentT(term, otherTerm, bounds + (binding -> otherBinding)))
            } yield termAE
          case term => Done(false)
        }
      case App(term, argument) =>
        other match {
          case App(otherTerm, otherArgument) =>
            for {
              termAE <- More(() => isAlphaEquivalentT(term, otherTerm, bounds))
              argumentAE <- More(() => isAlphaEquivalentT(argument, otherArgument, bounds))
            } yield termAE && argumentAE
          case term => Done(false)
        }
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
      case Abs(binding, term) =>
        val identifier: Identifier = binding.cloneIdentifier()
        for {
          currentTerm <- More(() => cloneTermT(term, mapping + (binding -> identifier)))
        } yield Abs(identifier, currentTerm)
      case App(term, argument) =>
        for {
          currentTerm <- More(() => cloneTermT(term, mapping))
          currentArgument <- More(() => cloneTermT(argument, mapping))
        } yield App(currentTerm, currentArgument)
    }

  def substitute(term: Term, variable: Identifier, result: Term): Term =
    substituteT(term, variable, result).runT

  def substituteT(term: Term, variable: Identifier, result: Term): Trampoline[Term] =
    term match {
      case Var(identifier) => if (identifier == variable) Done(cloneTerm(result)) else Done(Var(identifier))
      case Abs(binding, term) => if (binding == variable) Done(Abs(binding, term)) else for {
        currentTerm <- More(() => substituteT(term, variable, result))
      } yield Abs(binding, currentTerm)
      case App(term, argument) => for {
        currentTerm <- More(() => substituteT(term, variable, result))
        currentArgument <- More(() => substituteT(argument, variable, result))
      } yield App(currentTerm, currentArgument)
    }

  def analyzeTermSize(term: Term): Int =
    analyzeTermSizeT(term).runT

  def analyzeTermSizeT(term: Term): Trampoline[Int] =
    term match {
      case Var(identifier) => Done(1)
      case Abs(binding, term) => for {
        termSize <- More(() => analyzeTermSizeT(term))
      } yield 1 + termSize
      case App(term, argument) => for {
        termSize <- More(() => analyzeTermSizeT(term))
        argumentSize <- More(() => analyzeTermSizeT(argument))
      } yield 1 + termSize + argumentSize
    }

  def analyzeTermDepth(term: Term): Int =
    analyzeTermDepthT(term).runT

  def analyzeTermDepthT(term: Term): Trampoline[Int] =
    term match {
      case Var(identifier) => Done(1)
      case Abs(binding, term) => for {
        termSize <- More(() => analyzeTermDepthT(term))
      } yield 1 + termSize
      case App(term, argument) => for {
        termSize <- More(() => analyzeTermDepthT(term))
        argumentSize <- More(() => analyzeTermDepthT(argument))
      } yield 1 + math.max(termSize, argumentSize)
    }

  private def findUnusedName(name: String, usedNames: Set[String]): String =
    Stream.from(0).map({ name + _ }).filterNot(usedNames.contains).head

  private def boundedVariables(term: Term): Trampoline[Option[Set[Identifier]]] =
    term match {
      case Var(identifier) =>
        Done(Option(Set.empty))
      case Abs(binding, term) =>
        for {
          termBV <- More(() => boundedVariables(term))
        } yield for {
          termBounds <- termBV
          result <- if (termBounds.contains(binding)) Option.empty else Option(termBounds + binding)
        } yield result
      case App(term, argument) =>
        for {
          termBV <- More(() => boundedVariables(term))
          argumentBV <- More(() => boundedVariables(argument))
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
      case Abs(binding, term) =>
        More(() => isTermProperlyBounded(term, unbounded - binding))
      case App(term, argument) =>
        for {
          termPB <- More(() => isTermProperlyBounded(term, unbounded))
          argumentPB <- More(() => isTermProperlyBounded(argument, unbounded))
        } yield termPB && argumentPB
    }
}