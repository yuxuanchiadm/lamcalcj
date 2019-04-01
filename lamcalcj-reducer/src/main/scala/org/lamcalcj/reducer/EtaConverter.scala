package org.lamcalcj.reducer

import org.lamcalcj.ast.Lambda._
import org.lamcalcj.reducer.AbortReason._
import org.lamcalcj.utils.Utils
import org.lamcalcj.utils.Trampoline._

object EtaConverter {
  def etaConversion(term: Term, strategy: Strategy = Strategy()): Result = {
    val etaConverter: EtaConverter = new EtaConverter(strategy.maxStep, strategy.maxSize, strategy.maxDepth, strategy.headOnly, strategy.evaluationOnly)
    val result: Term = etaConverter(term).runT
    return Result(etaConverter.step, etaConverter.sizePeak, etaConverter.depthPeak, etaConverter.abortReason, result)
  }

  @throws[InterruptedException]
  def interruptibleEtaConversion(term: Term, strategy: Strategy = Strategy()): Result = {
    val etaConverter: EtaConverter = new EtaConverter(strategy.maxStep, strategy.maxSize, strategy.maxDepth, strategy.headOnly, strategy.evaluationOnly)
    val result: Term = etaConverter(term).interruptibleRunT
    return Result(etaConverter.step, etaConverter.sizePeak, etaConverter.depthPeak, etaConverter.abortReason, result)
  }

  private class EtaConverter(maxStep: Option[Int], maxSize: Option[Int], maxDepth: Option[Int], headOnly: Boolean, evaluationOnly: Boolean) {
    var step: Int = 0
    var size: Int = 0
    var sizePeak: Int = 0
    var depthPeak: Int = 0
    var abortReason: AbortReason = NormalForm

    def apply(originalTerm: Term): Trampoline[Term] =
      if (maxStep.exists(_ < step)) {
        abortReason = MaxStepReached
        Done(originalTerm)
      } else if (maxSize.exists(_ < originalTerm.size)) {
        abortReason = MaxSizeReached
        Done(originalTerm)
      } else if (maxDepth.exists(_ < originalTerm.depth)) {
        abortReason = MaxDepthReached
        Done(originalTerm)
      } else {
        size = originalTerm.size
        sizePeak = originalTerm.size
        depthPeak = originalTerm.depth
        More(() => convert(originalTerm, 0))
      }

    def convert(originalTerm: Term, depth: Int): Trampoline[Term] =
      if (abortReason != NormalForm)
        Done(originalTerm)
      else
        originalTerm match {
          case Var(identifier) => Done(originalTerm)
          case Abs(binding, App(term, Var(identifier))) if (binding == identifier && !Utils.hasFreeOccurrence(term, identifier)) => for {
            currentTerm <- More(() => convertEtaRedex(binding, term, depth))
            resultTerm <- More(() => convert(currentTerm, depth))
          } yield resultTerm
          case Abs(binding, term) => if (evaluationOnly) Done(originalTerm) else for {
            currentTerm <- More(() => convert(term, depth + 1))
            resultTerm <- currentTerm match {
              case App(term, Var(identifier)) if (binding == identifier && !Utils.hasFreeOccurrence(term, identifier)) => for {
                currentTerm <- More(() => convertEtaRedex(binding, term, depth))
                resultTerm <- More(() => convert(currentTerm, depth))
              } yield resultTerm
              case term => Done(Abs(binding, term))
            }
          } yield resultTerm
          case App(term, argument) => for {
            currentTerm <- More(() => convert(term, depth + 1))
            resultTerm <- if (headOnly) Done(App(currentTerm, argument)) else for {
              currentArgument <- More(() => convert(argument, depth + 1))
            } yield App(currentTerm, currentArgument)
          } yield resultTerm
        }

    def convertEtaRedex(binding: Identifier, term: Term, depth: Int): Trampoline[Term] =
      if (maxStep.exists(_ < step + 1)) {
        abortReason = MaxStepReached
        Done(Abs(binding, App(term, Var(binding))))
      } else for {
        resultTerm <- Done(term)
      } yield {
        val sizeDelta: Int = -3
        val depthDelta: Int = -2
        val resultSize = size + sizeDelta
        val resultDepth = depth + depthDelta
        if (maxSize.exists(_ < resultSize)) {
          abortReason = MaxSizeReached
          Abs(binding, App(term, Var(binding)))
        } else if (maxDepth.exists(_ < resultDepth)) {
          abortReason = MaxDepthReached
          Abs(binding, App(term, Var(binding)))
        } else {
          step += 1
          size = resultSize
          sizePeak = math.max(sizePeak, resultSize)
          depthPeak = math.max(depthPeak, resultDepth)
          resultTerm
        }
      }
  }
}