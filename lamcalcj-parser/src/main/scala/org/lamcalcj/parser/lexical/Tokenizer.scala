package org.lamcalcj.parser.lexical

import java.io.Reader

import org.lamcalcj.parser.lexical.Kind._
import org.lamcalcj.parser.lexical.TokenList._

import java.nio.CharBuffer
import java.util.Arrays

import scala.collection._

object Tokenizer {
  def tokenize(reader: Reader, tokenizerBehavior: TokenizerBehavior = new TokenizerBehavior()): Either[(Location, String), TokenList] = {
    val tokenize: Tokenizer = new Tokenizer(reader, tokenizerBehavior)
    val builder: TokenListBuilder = new TokenListBuilder

    var token: Token = null
    do {
      tokenize.beginToken()
      while (!tokenize.isTerminateState())
        tokenize.advanceState()
      token = tokenize.endToken().getOrElse(return Left(tokenize.errorToken()))
      builder += token
    } while (token.kind != EOF)

    return Right(builder.result())
  }

  private class Tokenizer(reader: Reader, tokenizerBehavior: TokenizerBehavior) {
    import NewlineState._

    var pushbackBuffer: CharBuffer = CharBuffer.allocate(256)
    val cpReader: CodePointReader = new CodePointReader(reader)
    var newlineState: NewlineState = None
    var currentLine: Int = 0
    var currentColumn: Int = 0
    var beginLine: Int = 0
    var beginColumn: Int = 0
    val image: StringBuilder = new StringBuilder
    val matchers: immutable.ListMap[Kind, TokenMatcher] = tokenizerBehavior.asListMap()
    val stateMap: mutable.Map[Kind, Int] = mutable.Map.empty
    var matchedTokenInfo: Option[(NewlineState, Int, Int, Int, Kind)] = Option.empty

    pushbackBuffer.flip()

    def read(): Int = {
      if (pushbackBuffer.hasRemaining()) {
        val c1: Char = pushbackBuffer.get()
        if (Character.isHighSurrogate(c1) && pushbackBuffer.hasRemaining()) {
          val c2: Char = pushbackBuffer.get()
          if (Character.isLowSurrogate(c2))
            return Character.toCodePoint(c1, c2)
          else
            pushbackBuffer.position(pushbackBuffer.position() - 1)
        }
        return c1
      } else {
        pushbackBuffer.limit(0)
        return cpReader.read()
      }
    }

    def next(): Int = {
      val cp: Int = read()
      newlineState match {
        case CR => if (cp != '\n') {
          currentLine += 1
          currentColumn = 0
        }
        case LF => {
          currentLine += 1
          currentColumn = 0
        }
        case None => {}
      }
      if (cp >= 0) {
        if (Character.isBmpCodePoint(cp))
          image += cp.asInstanceOf[Char]
        else {
          image += Character.highSurrogate(cp)
          image += Character.lowSurrogate(cp)
        }
        currentColumn += 1
        cp match {
          case '\r' => newlineState = CR
          case '\n' => newlineState = LF
          case _ => newlineState = None
        }
      }
      return cp
    }

    def beginToken(): Unit = {
      beginLine = currentLine
      beginColumn = currentColumn
      stateMap ++= matchers.mapValues(_.initialState)
      findMatchedToken()
    }

    def endToken(): Option[Token] = matchedTokenInfo.map {
      case (originNewlineState, line, column, imageLength, kind) =>
        if (pushbackBuffer.limit() == 0) {
          pushbackBuffer.clear()
          val remainingImage: String = image.substring(imageLength)
          if (remainingImage.length() > pushbackBuffer.remaining())
            pushbackBuffer = CharBuffer.allocate((for { bit <- (0 until 32) } yield (1 << bit) >>> 1)
              .find(_ >= remainingImage.length())
              .getOrElse(Int.MaxValue))
          pushbackBuffer.put(remainingImage)
          pushbackBuffer.flip()
        } else
          pushbackBuffer.position(pushbackBuffer.position() - image.length() + imageLength)
        newlineState = originNewlineState
        currentLine = line
        currentColumn = column
        val tokenImage: String = image.substring(0, imageLength)
        image.clear()
        stateMap.clear()
        matchedTokenInfo = Option.empty
        Token(kind, Location(beginLine, beginColumn, currentLine, currentColumn), tokenImage)
    }

    def errorToken(): (Location, String) =
      (Location(beginLine, beginColumn, currentLine, currentColumn), image.result())

    def advanceState(): Unit = {
      val cp: Int = next()
      stateMap transform { (kind, state) =>
        val terminateState: Int = matchers(kind).terminateState
        if (state == terminateState) terminateState else matchers(kind).nextState(state, cp)
      }
      findMatchedToken()
    }

    def isTerminateState(): Boolean = stateMap forall {
      case (kind, state) => state == matchers(kind).terminateState
    }

    def findMatchedToken(): Unit = {
      (for {
        (kind, matcher) <- matchers
        if matcher.finalStates contains stateMap(kind)
      } yield kind)
        .headOption
        .foreach(kind => matchedTokenInfo = Option((newlineState, currentLine, currentColumn, image.length, kind)))
    }
  }

  object NewlineState extends Enumeration {
    type NewlineState = Value

    val None, CR, LF = Value
  }
}
