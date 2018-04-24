package org.lamcalcj.reducer

import org.lamcalcj.ast.Lambda._
import org.lamcalcj.utils.Utils

object EtaConverter {
  def etaConversion(term: Term, maxStep: Int = Int.MaxValue, headOnly: Boolean = false, evaluationOnly: Boolean = false): (Boolean, Term) = {
    val etaConverter: EtaConverter = new EtaConverter(maxStep, headOnly, evaluationOnly)
    val result: Term = etaConverter.convert(term)
    return (!etaConverter.aborted, result)
  }

  private class EtaConverter(maxStep: Int, headOnly: Boolean, evaluationOnly: Boolean) {
    var step: Int = 0
    var aborted: Boolean = false

    def convert(inputTerm: Term): Term = if (aborted) inputTerm else
      inputTerm match {
        case Var(identifier) => Var(identifier)
        case Abs(variable, App(term, Var(identifier))) =>
          if (variable.identifier == identifier && !Utils.hasFreeOccurrence(term, identifier))
            if (step >= maxStep) {
              aborted = true
              inputTerm
            } else {
              step += 1
              term
            }
          else
            Abs(variable, App(if (evaluationOnly) term else convert(term), Var(identifier)))
        case Abs(variable, term) => Abs(variable, if (evaluationOnly) term else convert(term))
        case App(term, argument) => App(convert(term), if (headOnly) argument else convert(argument))
      }
  }
}