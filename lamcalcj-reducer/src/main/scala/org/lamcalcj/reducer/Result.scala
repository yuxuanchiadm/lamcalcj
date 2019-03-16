package org.lamcalcj.reducer

import org.lamcalcj.ast.Lambda.Term
import org.lamcalcj.reducer.AbortReason._

sealed case class Result(
  abortReason: AbortReason,
  step: Int,
  term: Term)
