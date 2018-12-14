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
          case Abs(variable, term) => if (!evaluationOnly)
            for {
              currentTerm <- reduce(term)
            } yield Abs(variable, currentTerm)
          else Done(Abs(variable, term))
          case App(term, argument) => reduceApp(term, argument, false)
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
          case Abs(variable, term) => reduceBetaRedex(variable, term, originalArgument) match {
            case Var(identifier) => Done(Var(identifier))
            case Abs(variable, term) => if (hasOuterArugment) Done(Abs(variable, term)) else reduce(Abs(variable, term))
            case App(term, argument) => reduceApp(term, argument, hasOuterArugment)
          }
          case App(term, argument) =>
            for {
              currentTerm <- reduceApp(term, argument, true)
              resultTerm <- currentTerm match {
                case Abs(variable, term) => reduceApp(Abs(variable, term), originalArgument, hasOuterArugment)
                case term => for {
                  currentTerm <- reduce(originalArgument)
                } yield App(term, if (!headOnly) currentTerm else originalArgument)
              }
            } yield resultTerm
        }

    def reduceBetaRedex(variable: Var, term: Term, argument: Term): Term = {
      if (step >= maxStep) {
        aborted = true
        App(Abs(variable, term), argument)
      } else {
        step += 1
        substitute(variable.identifier, term, argument)
      }
    }

    def substitute(id: Identifier, e: Term, r: Term): Term = e match {
      case Var(identifier) => if (identifier == id) Utils.cloneTerm(r) else Var(identifier)
      case Abs(variable, term) => Abs(variable, substitute(id, term, r))
      case App(term, argument) => App(substitute(id, term, r), substitute(id, argument, r))
    }
  }
}