package org.lamcalcj.parser.lexical

sealed trait TokenList {
  def prettyPrint(): String
}

object TokenList {
  case class Tail() extends TokenList {
    override def prettyPrint(): String = ""
  }
  case class Entry(special: TokenList, token: Token, next: TokenList) extends TokenList {
    override def prettyPrint(): String = special.prettyPrint + token.image + next.prettyPrint
  }

  val empty: TokenList = Tail()
}