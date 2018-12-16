package org.lamcalcj.pretty

import org.lamcalcj.ast.Lambda._
import org.lamcalcj.utils.Trampoline._
import org.lamcalcj.utils.Utils
import scala.collection.mutable.Builder

object PrettyPrint {
  def printLambda(
    term: Term,
    symbols: Symbols = Symbols(),
    omitRedundantGroup: Boolean = true,
    uncurryingAbstraction: Boolean = true,
    chainApplication: Boolean = true,
    enclosedTerm: Term => Boolean = _.isInstanceOf[Var]): String =
    printTerm(Utils.alphaConversion(term, Utils.freeVariables(term).map(_.name)), symbols, omitRedundantGroup, uncurryingAbstraction, chainApplication, enclosedTerm)

  def printTerm(
    term: Term,
    symbols: Symbols = Symbols(),
    omitRedundantGroup: Boolean = true,
    uncurryingAbstraction: Boolean = true,
    chainApplication: Boolean = true,
    enclosedTerm: Term => Boolean = _.isInstanceOf[Var]): String =
    new PrettyPrint(symbols, omitRedundantGroup, uncurryingAbstraction, chainApplication, enclosedTerm).printLambda(term, true).runT

  private class PrettyPrint(
    symbols: Symbols,
    omitRedundantGroup: Boolean,
    uncurryingAbstraction: Boolean,
    chainApplication: Boolean,
    enclosedTerm: Term => Boolean) {

    def printLambda(term: Term, enclosed: Boolean): Trampoline[String] =
      if (omitRedundantGroup && (enclosed || enclosedTerm(term)))
        printTerm(term)
      else
        for {
          termPP <- printTerm(term)
        } yield
          symbols.symbolGroupBegin + termPP + symbols.symbolGroupEnd

    def printTerm(term: Term): Trampoline[String] =
      term match {
        case Var(identifier) => Done(symbols.symbolVariableBegin + identifier.name + symbols.symbolVariableEnd)
        case Abs(variable, term) => printAbsTopLevel(Abs(variable, term))
        case App(term, argument) => printAppTopLevel(App(term, argument))
      }

    def printAbsTopLevel(term: Abs): Trampoline[String] =
      for {
        termPP <- if (uncurryingAbstraction) printAbsInnerLevel(term.term) else printLambda(term.term, true)
      } yield
        symbols.symbolAbstractionBegin +
        symbols.symbolArgumentsBegin +
        term.variable.identifier.name +
        ( if (uncurryingAbstraction)
            termPP
          else
            symbols.symbolArgumentsEnd +
            symbols.symbolAbstractionSeparator +
            termPP
        ) +
        symbols.symbolAbstractionEnd

    def printAbsInnerLevel(term: Term): Trampoline[String] =
      term match {
        case Abs(variable, term) =>
          for {
            termPP <- printAbsInnerLevel(term)
          } yield
            symbols.symbolArgumentsSeparator +
            variable.identifier.name +
            termPP
        case _ =>
          for {
            termPP <- printLambda(term, true)
          } yield
            symbols.symbolArgumentsEnd +
            symbols.symbolAbstractionSeparator +
            termPP
      }

    def printAppTopLevel(term: App): Trampoline[String] =
      for {
        termPP <- if (chainApplication) printAppInnerLevel(term.term) else printLambda(term.term, false)
        argumentPP <- printLambda(term.argument, false)
      } yield
        symbols.symbolApplyBegin +
        ( if (chainApplication)
            termPP
          else
            termPP +
            symbols.symbolApplySeparator
        ) +
        argumentPP +
        symbols.symbolApplyEnd

    def printAppInnerLevel(term: Term): Trampoline[String] =
      term match {
        case App(term, argument) =>
          for {
            termPP <- printAppInnerLevel(term)
            argumentPP <- printLambda(argument, false)
          } yield
            termPP +
            argumentPP +
            symbols.symbolApplySeparator
        case _ =>
          for {
            termPP <- printLambda(term, false)
          } yield
            termPP +
            symbols.symbolApplySeparator
      }
  }
}
