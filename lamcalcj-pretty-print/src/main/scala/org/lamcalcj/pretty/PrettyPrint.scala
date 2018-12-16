package org.lamcalcj.pretty

import org.lamcalcj.ast.Lambda._
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
    new PrettyPrint(symbols, omitRedundantGroup, uncurryingAbstraction, chainApplication, enclosedTerm).printLambda(term, true)

  private class PrettyPrint(
    symbols: Symbols,
    omitRedundantGroup: Boolean,
    uncurryingAbstraction: Boolean,
    chainApplication: Boolean,
    enclosedTerm: Term => Boolean) {

    def printLambda(term: Term, enclosed: Boolean): String =
      if (omitRedundantGroup && (enclosed || enclosedTerm(term)))
        printTerm(term)
      else
        symbols.symbolGroupBegin + printTerm(term) + symbols.symbolGroupEnd

    def printTerm(term: Term): String =
      term match {
        case Var(identifier) => symbols.symbolVariableBegin + identifier.name + symbols.symbolVariableEnd
        case Abs(variable, term) => printAbsTopLevel(Abs(variable, term))
        case App(term, argument) => printAppTopLevel(App(term, argument))
      }

    def printAbsTopLevel(term: Abs): String =
      symbols.symbolAbstractionBegin +
      symbols.symbolArgumentsBegin +
      term.variable.identifier.name +
      ( if (uncurryingAbstraction)
          printAbsInnerLevel(term.term)
        else
          symbols.symbolArgumentsEnd +
          symbols.symbolAbstractionSeparator +
          printLambda(term.term, true)
      ) +
      symbols.symbolAbstractionEnd

    def printAbsInnerLevel(term: Term): String =
      term match {
        case Abs(variable, term) =>
          symbols.symbolArgumentsSeparator +
          variable.identifier.name +
          printAbsInnerLevel(term)
        case _ =>
          symbols.symbolArgumentsEnd +
          symbols.symbolAbstractionSeparator +
          printLambda(term, true)
      }

    def printAppTopLevel(term: App): String =
      symbols.symbolApplyBegin +
      ( if (chainApplication)
          printAppInnerLevel(term.term)
        else
          printLambda(term.term, false) +
          symbols.symbolApplySeparator
      ) +
      printLambda(term.argument, false) +
      symbols.symbolApplyEnd

    def printAppInnerLevel(term: Term): String =
      term match {
        case App(term, argument) =>
          printAppInnerLevel(term) +
          printLambda(argument, false) +
          symbols.symbolApplySeparator
        case _ =>
          printLambda(term, false) +
          symbols.symbolApplySeparator
      }
  }
}
