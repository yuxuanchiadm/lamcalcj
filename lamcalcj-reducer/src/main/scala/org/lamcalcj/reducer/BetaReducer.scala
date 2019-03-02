package org.lamcalcj.reducer

import org.lamcalcj.ast.Lambda._
import org.lamcalcj.utils.Utils
import scala.collection.mutable.ArrayStack
import org.lamcalcj.utils.Trampoline._

object BetaReducer {
  def betaReduction(term: Term, maxStep: Option[Int] = Option(0xFF), headOnly: Boolean = false, evaluationOnly: Boolean = false): (Boolean, Term) = {
    val betaReducer: BetaReducer = new BetaReducer(maxStep, headOnly, evaluationOnly)
    val result: Term = betaReducer.reduce(term).runT
    return (!betaReducer.aborted, result)
  }

  private class BetaReducer(maxStep: Option[Int], headOnly: Boolean, evaluationOnly: Boolean) {
    var step: Int = 0
    var aborted: Boolean = false

    def reduce(originalTerm: Term): Trampoline[Term] =
      if (aborted)
        Done(originalTerm)
      else
        originalTerm match {
          case Var(identifier) => Done(originalTerm)
          case Abs(binding, term) => if (evaluationOnly) Done(originalTerm) else for {
            currentTerm <- More(() => reduce(term))
          } yield Abs(binding, currentTerm)
          case App(term, argument) => More(() => reduceApp(term, argument, false))
        }

    def reduceApp(originalTerm: Term, originalArgument: Term, hasOuterArugment: Boolean): Trampoline[Term] =
      if (aborted)
        Done(App(originalTerm, originalArgument))
      else
        originalTerm match {
          case Var(identifier) =>
            if (headOnly) Done(App(originalTerm, originalArgument)) else for {
              currentTerm <- More(() => reduce(originalArgument))
            } yield App(originalTerm, currentTerm)
          case Abs(binding, term) => for {
            currentTerm <- More(() => reduceBetaRedex(binding, term, originalArgument))
            resultTerm <- currentTerm match {
              case Var(identifier) => Done(currentTerm)
              case Abs(binding, term) => if (hasOuterArugment) Done(currentTerm) else More(() => reduce(currentTerm))
              case App(term, argument) => More(() => reduceApp(term, argument, hasOuterArugment))
            }
          } yield resultTerm
          case App(term, argument) =>
            for {
              currentTerm <- More(() => reduceApp(term, argument, true))
              resultTerm <- currentTerm match {
                case Abs(binding, term) => More(() => reduceApp(currentTerm, originalArgument, hasOuterArugment))
                case term => if (headOnly) Done(App(term, originalArgument)) else for {
                  currentTerm <- More(() => reduce(originalArgument))
                } yield App(term, currentTerm)
              }
            } yield resultTerm
        }

    def reduceBetaRedex(binding: Identifier, term: Term, argument: Term): Trampoline[Term] =
      if (maxStep.map(step >= _).getOrElse(false)) {
        aborted = true
        Done(App(Abs(binding, term), argument))
      } else {
        step += 1
        More(() => Utils.substituteT(term, binding, argument))
      }
  }
}