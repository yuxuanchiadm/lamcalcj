package org.lamcalcj.parser.lexical

sealed trait TokenList {
  def prettyPrint(): String

  def tokenMessage(): String
}

object TokenList {
  case class Tail() extends TokenList {
    override def prettyPrint(): String = ""

    override def tokenMessage(): String = "at end of token list"
  }
  case class Entry(special: TokenList, token: Token, next: TokenList) extends TokenList {
    override def prettyPrint(): String = special.prettyPrint + token.image + next.prettyPrint

    override def tokenMessage(): String = "at token of image: " + token.image +
      ". at location from [line " + token.location.beginLine + ", column " + token.location.beginColumn +
      "] to [line " + token.location.beginLine + ", column " + token.location.beginColumn + "]" +
      ". of kind " + token.kind
  }

  val empty: TokenList = Tail()
}