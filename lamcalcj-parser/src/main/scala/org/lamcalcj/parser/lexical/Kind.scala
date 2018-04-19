package org.lamcalcj.parser.lexical

object Kind {
  val EOF: Kind = KindEOF
  val Space: Kind = KindSpace
  val Abstract: Kind = KindAbstract
  val Delimiter: Kind = KindDelimiter
  val Identifier: Kind = KindIdentifier
  val Begin: Kind = KindBegin
  val End: Kind = KindEnd
}

abstract class Kind(val name: String, val isSpecial: Boolean) {
  override def toString(): String = name
}
private object KindEOF extends Kind("EOF", false)
private object KindSpace extends Kind("Space", true)
private object KindAbstract extends Kind("Abstract", false)
private object KindDelimiter extends Kind("Delimiter", false)
private object KindIdentifier extends Kind("Identifier", false)
private object KindBegin extends Kind("Begin", false)
private object KindEnd extends Kind("End", false)
