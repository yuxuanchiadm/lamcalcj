package org.lamcalcj.parser.lexical

import java.io.Reader
import java.io.PushbackReader
import scala.collection.mutable.ListBuffer

import org.lamcalcj.parser.lexical.Kind._
import org.lamcalcj.parser.lexical.TokenList._

object Tokenizer {
  def tokenize(reader: Reader): Either[(Location, String), TokenList] = {
    val tokenizer: Tokenizer = new Tokenizer(new CodePointReader(reader))
    import tokenizer._;

    val builder: TokenListBuilder = new TokenListBuilder

    var state: Int = 0
    while (true) state match {
      case 0 => {
        beginToken()
        val cp: Int = next()
        if (cp == -1) {
          builder += endToken(EOF)
          state = -1
        } else if (cp == ' ' | cp == '\r' | cp == '\n' | cp == '\t') {
          state = 1
        } else if (cp == 'λ') {
          builder += endToken(Abstract)
          state = 0
        } else if (cp == '.') {
          builder += endToken(Delimiter)
          state = 0
        } else if (Character.isLetter(cp) || Character.isDigit(cp)
          || Character.getType(cp) == Character.MATH_SYMBOL || Character.getType(cp) == Character.OTHER_SYMBOL
          || cp == '$' || cp == '_') {
          state = 2
        } else if (cp == '(') {
          builder += endToken(Begin)
          state = 0
        } else if (cp == ')') {
          builder += endToken(End)
          state = 0
        } else {
          state = -2
        }
      }
      case 1 => {
        val cp: Int = peek()
        if (cp == ' ' | cp == '\r' | cp == '\n' | cp == '\t') {
          next()
          state = 1
        } else {
          builder += endToken(Space)
          state = 0
        }
      }
      case 2 => {
        val cp: Int = peek()
        if (Character.isLetter(cp) || Character.isDigit(cp)
          || Character.getType(cp) == Character.MATH_SYMBOL || Character.getType(cp) == Character.OTHER_SYMBOL
          || cp == '$' || cp == '_') {
          next()
          state = 2
        } else {
          builder += endToken(Identifier)
          state = 0
        }
      }
      case -1 =>
        return Right(builder.result())
      case -2 =>
        return Left(errorToken())
    }
    throw new IllegalStateException
  }

  private class Tokenizer(reader: CodePointReader) {
    private var eof: Boolean = false
    private var currentLine: Int = 0
    private var currentColumn: Int = 0
    private var beginLine: Int = 0
    private var beginColumn: Int = 0
    private val image: StringBuilder = new StringBuilder

    def beginToken(): Unit = {
      beginLine = currentLine
      beginColumn = currentColumn
      image.clear()
    }

    def endToken(kind: Kind): Token =
      Token(kind, Location(beginLine, beginColumn, currentLine, currentColumn), image.toString())

    def errorToken(): (Location, String) =
      (Location(beginLine, beginColumn, currentLine, currentColumn), image.toString())

    def next(): Int = {
      if (eof)
        return -1
      val cp: Int = reader.read()
      if (cp >= 0)
        image ++= String.valueOf(Character.toChars(cp))
      if (cp == '\n' || (cp == '\r' && peek() != '\n')) {
        currentLine += 1
        currentColumn = 0
      } else if (cp >= 0) {
        currentColumn += 1
      } else {
        eof = true
      }
      return cp
    }

    def peek(): Int = {
      val cp: Int = reader.read()
      if (cp >= 0)
        reader.unread(cp)
      return cp
    }
  }

  private class CodePointReader(originReader: Reader) {
    private val reader: PushbackReader = new PushbackReader(originReader, 2)

    def unread(cp: Int): Unit = {
      reader.unread(Character.toChars(cp))
    }

    def read(): Int = {
      val i1: Int = reader.read()
      if (i1 < 0)
        return i1
      val c1: Char = i1.asInstanceOf[Char]
      if (Character.isHighSurrogate(c1)) {
        val i2: Int = reader.read()
        if (i2 >= 0) {
          val c2: Char = i2.asInstanceOf[Char]
          if (Character.isLowSurrogate(c2))
            return Character.toCodePoint(c1, c2)
          else
            reader.unread(i2)
        }
      }
      return c1
    }
  }
}
