package org.lamcalcj.reducer

import org.lamcalcj.ast.Lambda.Term
import org.lamcalcj.reducer.AbortReason._

sealed case class Result(
  step: Int,
  sizePeak: Int,
  depthPeak: Int,
  abortReason: AbortReason,
  term: Term)
