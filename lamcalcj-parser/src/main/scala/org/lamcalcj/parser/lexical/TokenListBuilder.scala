package org.lamcalcj.parser.lexical

import org.lamcalcj.parser.lexical.TokenList._

class TokenListBuilder {
  var tokenList: List[(List[Token], Token)] = List.empty
  var specialList: List[Token] = List.empty

  def +=(token: Token): this.type = {
    if (token.kind.isSpecial)
      specialList = token :: specialList
    else {
      tokenList = (specialList, token) :: tokenList
      specialList = List.empty
    }
    return this
  }

  def ++=(tokens: TokenList): this.type = {
    tokens match {
      case Tail() => ()
      case Entry(special, token, next) => {
        this ++= special
        this += token
        this ++= next
      }
    }
    return this
  }

  def result(): TokenList =
    tokenList.foldLeft(TokenList.empty) {
      case (next, (special, token)) => Entry(buildSpecial(special), token, next)
    }

  private def buildSpecial(specialList: List[Token]): TokenList =
    specialList.foldLeft(TokenList.empty) {
      (next, token) => Entry(TokenList.empty, token, next)
    }
}
