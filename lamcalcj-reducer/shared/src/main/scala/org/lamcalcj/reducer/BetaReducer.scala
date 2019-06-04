package org.lamcalcj.reducer

import org.lamcalcj.ast.Lambda._
import org.lamcalcj.reducer.AbortReason._
import org.lamcalcj.utils.Utils
import org.lamcalcj.utils.Trampoline._
import scala.collection.mutable.ArrayStack

object BetaReducer {
  def betaReduction(term: Term, strategy: Strategy = Strategy()): Result = {
    val betaReducer: BetaReducer = new BetaReducer(strategy.maxStep, strategy.maxSize, strategy.maxDepth, strategy.headOnly, strategy.evaluationOnly)
    val result: Term = betaReducer(term).runT
    Result(betaReducer.step, betaReducer.sizePeak, betaReducer.depthPeak, betaReducer.abortReason, result)
  }

  @throws[InterruptedException]
  def interruptibleBetaReduction(term: Term, strategy: Strategy = Strategy()): Result = {
    val betaReducer: BetaReducer = new BetaReducer(strategy.maxStep, strategy.maxSize, strategy.maxDepth, strategy.headOnly, strategy.evaluationOnly)
    val result: Term = betaReducer(term).interruptibleRunT
    Result(betaReducer.step, betaReducer.sizePeak, betaReducer.depthPeak, betaReducer.abortReason, result)
  }

  private class BetaReducer(maxStep: Option[Int], maxSize: Option[Int], maxDepth: Option[Int], headOnly: Boolean, evaluationOnly: Boolean) {
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
        More(() => reduce(originalTerm, 0))
      }

    def reduce(originalTerm: Term, depth: Int): Trampoline[Term] =
      if (abortReason != NormalForm)
        Done(originalTerm)
      else
        originalTerm match {
          case Var(identifier) => Done(originalTerm)
          case Abs(binding, term) => if (evaluationOnly) Done(originalTerm) else for {
            currentTerm <- More(() => reduce(term, depth + 1))
          } yield Abs(binding, currentTerm)
          case App(term, argument) => More(() => reduceApp(term, argument, false, depth))
        }

    def reduceApp(originalTerm: Term, originalArgument: Term, hasOuterArgument: Boolean, depth: Int): Trampoline[Term] =
      if (abortReason != NormalForm)
        Done(App(originalTerm, originalArgument))
      else
        originalTerm match {
          case Var(identifier) =>
            if (headOnly) Done(App(originalTerm, originalArgument)) else for {
              currentTerm <- More(() => reduce(originalArgument, depth + 1))
            } yield App(originalTerm, currentTerm)
          case Abs(binding, term) => for {
            currentTerm <- More(() => reduceBetaRedex(binding, term, originalArgument, depth))
            resultTerm <- currentTerm match {
              case Var(identifier) => Done(currentTerm)
              case Abs(binding, term) => if (hasOuterArgument) Done(currentTerm) else More(() => reduce(currentTerm, depth))
              case App(term, argument) => More(() => reduceApp(term, argument, hasOuterArgument, depth))
            }
          } yield resultTerm
          case App(term, argument) => for {
            currentTerm <- More(() => reduceApp(term, argument, true, depth + 1))
            resultTerm <- currentTerm match {
              case Abs(binding, term) => More(() => reduceApp(currentTerm, originalArgument, hasOuterArgument, depth))
              case term => if (headOnly) Done(App(term, originalArgument)) else for {
                currentTerm <- More(() => reduce(originalArgument, depth + 1))
              } yield App(term, currentTerm)
            }
          } yield resultTerm
        }

    def reduceBetaRedex(binding: Identifier, term: Term, argument: Term, depth: Int): Trampoline[Term] =
      if (maxStep.exists(_ < step + 1)) {
        abortReason = MaxStepReached
        Done(App(Abs(binding, term), argument))
      } else for {
        resultTerm <- More(() => Utils.substituteT(term, binding, argument))
      } yield {
        val sizeDelta: Int = resultTerm.size - (1 + (1 + term.size) + argument.size)
        val depthDelta: Int = resultTerm.depth - (1 + math.max(1 + term.depth, argument.depth))
        val resultSize = size + sizeDelta
        val resultDepth = depth + depthDelta
        if (maxSize.exists(_ < resultSize)) {
          abortReason = MaxSizeReached
          App(Abs(binding, term), argument)
        } else if (maxDepth.exists(_ < resultDepth)) {
          abortReason = MaxDepthReached
          App(Abs(binding, term), argument)
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