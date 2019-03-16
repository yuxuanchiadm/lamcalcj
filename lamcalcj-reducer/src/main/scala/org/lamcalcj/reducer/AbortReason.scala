package org.lamcalcj.reducer

object AbortReason extends Enumeration {
  sealed class Val private[AbortReason] (val successful: Boolean) extends super.Val

  type AbortReason = Val

  val NormalForm = new Val(true)
  val MaxStepReached = new Val(false)
  val MaxSizeReached = new Val(false)
  val MaxDepthReached = new Val(false)
}
