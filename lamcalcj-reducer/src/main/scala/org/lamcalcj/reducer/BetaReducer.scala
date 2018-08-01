package org.lamcalcj.reducer

import org.lamcalcj.ast.Lambda._
import org.lamcalcj.utils.Utils
import scala.collection.mutable.ArrayStack

object BetaReducer {
  def betaReduction(term: Term, maxStep: Int = 0xFF, headOnly: Boolean = false, evaluationOnly: Boolean = false): (Boolean, Term) = {
    val betaReducer: BetaReducer = new BetaReducer(maxStep, headOnly, evaluationOnly)
    val result: Term = betaReducer.reduce(term)
    return (!betaReducer.aborted, result)
  }

  private class BetaReducer(maxStep: Int, headOnly: Boolean, evaluationOnly: Boolean) {
    var step: Int = 0
    var aborted: Boolean = false

    def reduce(inputTerm: Term): Term = {
      if (aborted)
        return inputTerm
      var currentTerm: Term = inputTerm

      val variableStack: ArrayStack[Var] = new ArrayStack
      val argumentStack: ArrayStack[Term] = new ArrayStack
      do {
        if (!evaluationOnly) do {} while (currentTerm match {
          case Abs(variable, term) => {
            variableStack.push(variable)
            currentTerm = term
            true
          }
          case _ => false
        })
        do {} while (currentTerm match {
          case App(term, argument) => {
            argumentStack.push(argument)
            currentTerm = term
            true
          }
          case _ => false
        })
        do {} while (!aborted && argumentStack.nonEmpty && (currentTerm match {
          case Abs(variable, term) => {
            if (step >= maxStep)
              aborted = true
            else {
              step += 1
              currentTerm = substitute(term, variable.identifier, argumentStack.pop())
            }
            true
          }
          case _ => false
        }))
      } while (!aborted && (currentTerm match {
        case Var(_) => false
        case Abs(_, _) => !evaluationOnly
        case App(_, _) => true
      }))
      while (argumentStack.nonEmpty) {
        val argument: Term = argumentStack.pop()
        currentTerm = App(currentTerm, if (!headOnly) reduce(argument) else argument)
      }
      while (variableStack.nonEmpty) {
        val variable: Var = variableStack.pop()
        currentTerm = Abs(variable, currentTerm)
      }
      return currentTerm
    }

    def substitute(e: Term, id: Identifier, r: Term): Term = e match {
      case Var(identifier) => if (identifier == id) Utils.cloneTerm(r) else Var(identifier)
      case Abs(variable, term) => Abs(variable, substitute(term, id, r))
      case App(term, argument) => App(substitute(term, id, r), substitute(argument, id, r))
    }
  }
}