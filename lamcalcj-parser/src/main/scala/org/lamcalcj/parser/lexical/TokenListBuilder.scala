package org.lamcalcj.parser.lexical

import org.lamcalcj.parser.lexical.TokenList._

class TokenListBuilder {
  var tokenList: List[(List[Token], Token)] = List.empty
  var specialList: List[Token] = List.empty

  def +=(token: Token): Unit =
    if (token.kind.isSpecial)
      specialList = token :: specialList
    else {
      tokenList = (specialList, token) :: tokenList
      specialList = List.empty
    }

  def result(): TokenList =
    (TokenList.empty /: tokenList) {
      case (next, (special, token)) => Entry(buildSpecial(special), token, next)
    }

  private def buildSpecial(specialList: List[Token]): TokenList =
    (TokenList.empty /: specialList) {
      (special, token) => Entry(special, token, TokenList.empty)
    }
}
