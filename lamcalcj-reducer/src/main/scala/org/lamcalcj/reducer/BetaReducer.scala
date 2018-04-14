package org.lamcalcj.reducer

import org.lamcalcj.ast.Lambda._
import org.lamcalcj.utils.Utils

object BetaReducer {
  def betaReduction(term: Term, maxStep: Int = 0xFF, headOnly: Boolean = false, evaluationOnly: Boolean = false): (Boolean, Term) = {
    val betaReducer: BetaReducer = new BetaReducer(maxStep, headOnly, evaluationOnly)
    val result: Term = betaReducer.reduce(term)
    return (!betaReducer.aborted, result)
  }

  private class BetaReducer(maxStep: Int, headOnly: Boolean, evaluationOnly: Boolean) {
    var step: Int = 0
    var aborted: Boolean = false

    def reduce(term: Term): Term = if (step >= maxStep) { aborted = true; term } else
      term match {
        case Var(identifier) => Var(identifier)
        case Abs(variable, term) =>
          Abs(variable, if (evaluationOnly) term else reduce(term))
        case App(Var(identity), argumnet) => App(Var(identity), if (headOnly) argumnet else reduce(argumnet))
        case App(Abs(variable, term), argument) => { step += 1; reduce(substitute(term, variable.identifier, argument)) }
        case App(term, argument) => reduce(term) match {
          case Abs(variable, term) => reduce(App(Abs(variable, term), argument))
          case term => App(term, if (headOnly) argument else reduce(argument))
        }
      }

    def substitute(e: Term, id: Identifier, r: Term): Term = e match {
      case Var(identifier) => if (identifier == id) Utils.cloneTerm(r) else Var(identifier)
      case Abs(variable, term) => Abs(variable, substitute(term, id, r))
      case App(term, argument) => App(substitute(term, id, r), substitute(argument, id, r))
    }
  }
}