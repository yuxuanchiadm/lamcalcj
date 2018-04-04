package org.lamcalcj.parser.lexical

object Kind extends Enumeration {
  type Kind = Value

  val EOF = Value
  val Space = Value
  val Abstract = Value
  val Delimiter = Value
  val Identifier = Value
  val Begin = Value
  val End = Value
}
