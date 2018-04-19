package org.lamcalcj.parser.lexical

import java.io.Reader

class CodePointReader(reader: Reader) {
  private var c: Int = -1

  private def readChar(): Int = {
    if (c < 0)
      return reader.read()
    val result: Int = c
    c = -1
    return result
  }

  private def unreadChar(c: Int): Unit = {
    if (c >= 0)
      throw new IllegalStateException
    this.c = c
  }

  def read(): Int = {
    val i1: Int = readChar()
    if (i1 < 0)
      return i1
    val c1: Char = i1.asInstanceOf[Char]
    if (Character.isHighSurrogate(c1)) {
      val i2: Int = readChar()
      if (i2 >= 0) {
        val c2: Char = i2.asInstanceOf[Char]
        if (Character.isLowSurrogate(c2))
          return Character.toCodePoint(c1, c2)
        else
          unreadChar(i2)
      }
    }
    return c1
  }
}
