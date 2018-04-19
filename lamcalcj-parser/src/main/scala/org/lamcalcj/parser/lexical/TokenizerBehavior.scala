package org.lamcalcj.parser.lexical

import org.lamcalcj.parser.lexical.Kind._
import scala.collection.immutable.ListMap

class TokenizerBehavior(
  tokenEOFMatcher: TokenMatcher = new CodePointTokenMatcher(-1),
  tokenSpaceMatcher: TokenMatcher = new RepeatConditionTokenMatcher(cp => cp == ' ' | cp == '\r' | cp == '\n' | cp == '\t'),
  tokenAbstractMatcher: TokenMatcher = new CodePointTokenMatcher('λ'),
  tokenDelimiterMatcher: TokenMatcher = new CodePointTokenMatcher('.'),
  tokenIdentifierMatcher: TokenMatcher = new RepeatConditionTokenMatcher(cp => cp != 'λ' && (Character.isLetter(cp) || Character.isDigit(cp) ||
    Character.getType(cp) == Character.MATH_SYMBOL || Character.getType(cp) == Character.OTHER_SYMBOL || cp == '$' || cp == '_')),
  tokenBeginMatcher: TokenMatcher = new CodePointTokenMatcher('('),
  tokenEndMatcher: TokenMatcher = new CodePointTokenMatcher(')')) {

  def asListMap(): ListMap[Kind, TokenMatcher] = ListMap(
    EOF -> tokenEOFMatcher,
    Space -> tokenSpaceMatcher,
    Abstract -> tokenAbstractMatcher,
    Delimiter -> tokenDelimiterMatcher,
    Identifier -> tokenIdentifierMatcher,
    Begin -> tokenBeginMatcher,
    End -> tokenEndMatcher)
}
