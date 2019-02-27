package org.lamcalcj.reducer

import org.lamcalcj.ast.Lambda._
import org.lamcalcj.utils.Utils
import scala.collection.mutable.ArrayStack
import org.lamcalcj.utils.Trampoline._

object BetaReducer {
  def betaReduction(term: Term, maxStep: Int = 0xFF, headOnly: Boolean = false, evaluationOnly: Boolean = false): (Boolean, Term) = {
    val betaReducer: BetaReducer = new BetaReducer(maxStep, headOnly, evaluationOnly)
    val result: Term = betaReducer.reduce(term).runT
    return (!betaReducer.aborted, result)
  }

  private class BetaReducer(maxStep: Int, headOnly: Boolean, evaluationOnly: Boolean) {
    var step: Int = 0
    var aborted: Boolean = false

    def reduce(originalTerm: Term): Trampoline[Term] =
      if (aborted)
        Done(originalTerm)
      else
        originalTerm match {
          case Var(identifier) => Done(Var(identifier))
          case Abs(binding, term) => if (!evaluationOnly)
            for {
              currentTerm <- reduce(term)
            } yield Abs(binding, currentTerm)
          else Done(Abs(binding, term))
          case App(term, argument) => More(() => reduceApp(term, argument, false))
        }

    def reduceApp(originalTerm: Term, originalArgument: Term, hasOuterArugment: Boolean): Trampoline[Term] =
      if (aborted)
        Done(App(originalTerm, originalArgument))
      else
        originalTerm match {
          case Var(identifier) =>
            for {
              currentTerm <- reduce(originalArgument)
            } yield App(Var(identifier), if (!headOnly) currentTerm else originalArgument)
          case Abs(binding, term) => reduceBetaRedex(binding, term, originalArgument) match {
            case Var(identifier) => Done(Var(identifier))
            case Abs(binding, term) => if (hasOuterArugment) Done(Abs(binding, term)) else More(() => reduce(Abs(binding, term)))
            case App(term, argument) => More(() => reduceApp(term, argument, hasOuterArugment))
          }
          case App(term, argument) =>
            for {
              currentTerm <- More(() => reduceApp(term, argument, true))
              resultTerm <- currentTerm match {
                case Abs(binding, term) => More(() => reduceApp(Abs(binding, term), originalArgument, hasOuterArugment))
                case term => for {
                  currentTerm <- reduce(originalArgument)
                } yield App(term, if (!headOnly) currentTerm else originalArgument)
              }
            } yield resultTerm
        }

    def reduceBetaRedex(binding: Identifier, term: Term, argument: Term): Term = {
      if (step >= maxStep) {
        aborted = true
        App(Abs(binding, term), argument)
      } else {
        step += 1
        substitute(binding, term, argument).runT
      }
    }

    def substitute(originalIdentifier: Identifier, originalTerm: Term, originalArgument: Term): Trampoline[Term] = originalTerm match {
      case Var(identifier) => if (identifier == originalIdentifier) Done(Utils.cloneTerm(originalArgument)) else Done(Var(identifier))
      case Abs(binding, term) => for {
        currentTerm <- More(() => substitute(originalIdentifier, term, originalArgument))
      } yield Abs(binding, currentTerm)
      case App(term, argument) => for {
        currentTerm <- More(() => substitute(originalIdentifier, term, originalArgument))
        currentArgument <- More(() => substitute(originalIdentifier, argument, originalArgument))
      } yield App(currentTerm, currentArgument)
    }
  }
}