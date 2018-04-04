package org.lamcalcj.parser.lexical

object Kind extends Enumeration {
  protected case class Val(isSpecial: Boolean) extends super.Val
  
  type Kind = Val

  val EOF = Val(false)
  val Space = Val(true)
  val Abstract = Val(false)
  val Delimiter = Val(false)
  val Identifier = Val(false)
  val Begin = Val(false)
  val End = Val(false)
}
