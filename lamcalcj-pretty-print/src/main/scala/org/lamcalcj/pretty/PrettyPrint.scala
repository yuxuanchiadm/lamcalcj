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
        case Abs(variable, term) => {
          if (uncurryingAbstraction) {
            val builder: StringBuilder = new StringBuilder()
            builder ++= symbols.symbolAbstractionBegin
            builder ++= symbols.symbolArgumentsBegin
            builder ++= variable.identifier.name
            var current: Term = term
            do {} while (current match {
              case Abs(variable, term) => {
                builder ++= symbols.symbolArgumentsSeparator
                builder ++= variable.identifier.name
                current = term
                true
              }
              case _ => false
            })
            builder ++= symbols.symbolArgumentsEnd
            builder ++= symbols.symbolAbstractionSeparator
            builder ++= printLambda(current, true)
            builder ++= symbols.symbolAbstractionEnd
            builder.toString
          } else symbols.symbolAbstractionBegin + symbols.symbolArgumentsBegin + variable.identifier.name + symbols.symbolArgumentsEnd +
            symbols.symbolAbstractionSeparator + printLambda(term, true) + symbols.symbolAbstractionEnd
        }
        case App(term, argument) => {
          if (chainApplication) {
            val builder: StringBuilder = new StringBuilder()
            builder ++= symbols.symbolApplyBegin
            var applicationChain: List[Term] = List.empty
            var current: Term = term
            do {} while (current match {
              case App(term, argument) => {
                applicationChain = argument :: applicationChain
                current = term
                true
              }
              case term => {
                applicationChain = term :: applicationChain
                false
              }
            })
            applicationChain.foreach(term => {
              builder ++= printLambda(term, false)
              builder ++= symbols.symbolApplySeparator
            })
            builder ++= printLambda(argument, false)
            builder ++= symbols.symbolApplyEnd
            builder.toString
          } else symbols.symbolApplyBegin + printLambda(term, false) + symbols.symbolApplySeparator + printLambda(argument, false) +
            symbols.symbolApplyEnd
        }
      }
  }
}
