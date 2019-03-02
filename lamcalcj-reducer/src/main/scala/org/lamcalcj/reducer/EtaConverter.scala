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

    def convert(originalTerm: Term): Trampoline[Term] =
      if (aborted)
        Done(originalTerm)
      else
        originalTerm match {
          case Var(identifier) => Done(originalTerm)
          case Abs(binding, App(term, Var(identifier))) =>
            if (binding == identifier && !Utils.hasFreeOccurrence(term, identifier))
              convertEtaRedex(binding, term)
            else if (evaluationOnly) Done(originalTerm) else for {
              currentTerm <- More(() => convert(term))
            } yield Abs(binding, App(currentTerm, Var(identifier)))
          case Abs(binding, term) => if (evaluationOnly) Done(originalTerm) else for {
            currentTerm <- More(() => convert(term))
          } yield Abs(binding, currentTerm)
          case App(term, argument) => for {
            currentTerm <- More(() => convert(term))
            resultTerm <- if (headOnly) Done(App(currentTerm, argument)) else for {
              currentArgument <- More(() => convert(argument))
            } yield App(currentTerm, currentArgument)
          } yield resultTerm
        }

    def convertEtaRedex(binding: Identifier, term: Term): Trampoline[Term] =
      if (maxStep.map(step >= _).getOrElse(false)) {
        aborted = true
        Done(Abs(binding, App(term, Var(binding))))
      } else {
        step += 1
        More(() => convert(term))
      }
  }
}