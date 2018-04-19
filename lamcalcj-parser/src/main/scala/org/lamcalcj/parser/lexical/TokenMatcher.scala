package org.lamcalcj.parser.lexical

class CodePointTokenMatcher(cp: Int) extends TokenMatcher {
  override val finalStates = Set(1)

  override def nextState(state: Int, cp: Int): Int = state match {
    case 0 => cp match {
      case this.cp => 1
      case _ => terminateState
    }
    case 1 => cp match {
      case _ => terminateState
    }
    case terminateState => terminateState
  }
}

class StringTokenMatcher(string: String) extends TokenMatcher {
  val codePoints: Array[Int] = string.codePoints.toArray

  override val finalStates = Set(codePoints.length)

  override def nextState(state: Int, cp: Int): Int =
    if (state == terminateState)
      terminateState
    else if (state >= 0 && state < codePoints.length)
      if (cp == codePoints(state))
        state + 1
      else
        terminateState
    else if (state == codePoints.length)
      terminateState
    else
      throw new IllegalStateException
}

class RepeatConditionTokenMatcher(condition: Int => Boolean) extends TokenMatcher {
  override val finalStates = Set(1)

  override def nextState(state: Int, cp: Int): Int = state match {
    case 0 =>
      if (condition(cp))
        1
      else
        terminateState
    case 1 =>
      if (condition(cp))
        1
      else
        terminateState
    case terminateState => terminateState
  }
}

trait TokenMatcher {
  val initialState: Int = 0
  val terminateState: Int = -1
  val finalStates: Set[Int]

  def nextState(state: Int, cp: Int): Int
}
