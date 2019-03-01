package org.lamcalcj.reducer

import org.lamcalcj.ast.Lambda._
import org.lamcalcj.utils.Utils
import org.lamcalcj.utils.Trampoline._

object EtaConverter {
  def etaConversion(term: Term, maxStep: Option[Int] = Option(Int.MaxValue), headOnly: Boolean = false, evaluationOnly: Boolean = false): (Boolean, Term) = {
    val etaConverter: EtaConverter = new EtaConverter(maxStep, headOnly, evaluationOnly)
    val result: Term = etaConverter.convert(term).runT
    return (!etaConverter.aborted, result)
  }

  private class EtaConverter(maxStep: Option[Int], headOnly: Boolean, evaluationOnly: Boolean) {
    var step: Int = 0
    var aborted: Boolean = false

    def convert(originalTerm: Term): Trampoline[Term] = if (aborted) Done(originalTerm) else
      originalTerm match {
        case Var(identifier) => Done(Var(identifier))
        case Abs(binding, App(term, Var(identifier))) =>
          if (binding == identifier && !Utils.hasFreeOccurrence(term, identifier))
            if (maxStep.map(step >= _).getOrElse(false)) {
              aborted = true
              Done(originalTerm)
            } else {
              step += 1
              More(() => convert(term))
            }
          else for {
            currentTerm <- More(() => convert(term))
          } yield Abs(binding, App(if (evaluationOnly) term else currentTerm, Var(identifier)))
        case Abs(binding, term) => for {
          currentTerm <- More(() => convert(term))
        } yield Abs(binding, if (evaluationOnly) term else currentTerm)
        case App(term, argument) => for {
          currentTerm <- More(() => convert(term))
          currentArgument <- More(() => convert(argument))
        } yield App(currentTerm, if (headOnly) argument else currentArgument)
      }
  }
}