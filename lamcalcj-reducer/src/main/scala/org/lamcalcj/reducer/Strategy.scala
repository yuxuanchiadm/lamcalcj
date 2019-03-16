package org.lamcalcj.reducer

sealed case class Strategy(
  maxStep: Option[Int] = Option.empty,
  maxSize: Option[Int] = Option.empty,
  maxDepth: Option[Int] = Option.empty,
  headOnly: Boolean = false,
  evaluationOnly: Boolean = false)
