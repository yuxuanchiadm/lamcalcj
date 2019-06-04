package org.lamcalcj.reducer

object AbortReason extends Enumeration {
  sealed class Val private[AbortReason](name: String, val successful: Boolean) extends super.Val(name)

  type AbortReason = Val

  val NormalForm = new Val("NormalForm", true)
  val MaxStepReached = new Val("MaxStepReached", false)
  val MaxSizeReached = new Val("MaxSizeReached", false)
  val MaxDepthReached = new Val("MaxDepthReached", false)
}
