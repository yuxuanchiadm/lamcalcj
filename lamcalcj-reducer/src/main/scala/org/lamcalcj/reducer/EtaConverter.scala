package org.lamcalcj.reducer

import org.lamcalcj.ast.Lambda._
import org.lamcalcj.utils.Utils
import org.lamcalcj.utils.Trampoline._

object EtaConverter {
  def etaConversion(term: Term, maxStep: Int = Int.MaxValue, headOnly: Boolean = false, evaluationOnly: Boolean = false): (Boolean, Term) = {
    val etaConverter: EtaConverter = new EtaConverter(maxStep, headOnly, evaluationOnly)
    val result: Term = etaConverter.convert(term).runT
    return (!etaConverter.aborted, result)
  }

  private class EtaConverter(maxStep: Int, headOnly: Boolean, evaluationOnly: Boolean) {
    var step: Int = 0
    var aborted: Boolean = false

    def convert(inputTerm: Term): Trampoline[Term] = if (aborted) Done(inputTerm) else
      inputTerm match {
        case Var(identifier) => Done(Var(identifier))
        case Abs(variable, App(term, Var(identifier))) =>
          if (variable.identifier == identifier && !Utils.hasFreeOccurrence(term, identifier))
            if (step >= maxStep) {
              aborted = true
              Done(inputTerm)
            } else {
              step += 1
              Done(term)
            }
          else for {
            currentTerm <- convert(term)
          } yield Abs(variable, App(if (evaluationOnly) term else currentTerm, Var(identifier)))
        case Abs(variable, term) => for {
          currentTerm <- convert(term)
        } yield Abs(variable, if (evaluationOnly) term else currentTerm)
        case App(term, argument) => for {
          currentTerm <- convert(term)
          currentArgument <- convert(argument)
        } yield App(currentTerm, if (headOnly) argument else currentArgument)
      }
  }
}