package org.lamcalcj.parser

import org.lamcalcj.parser.Parser._
import org.lamcalcj.utils.Trampoline._
import scala.util.hashing.MurmurHash3

sealed trait Text {
  def empty: Boolean

  def length: Int

  def toString: String

  def ++(t: Text): Text

  def head: Option[Char]

  def tail: Option[Text]

  def uncons: Option[(Char, Text)]

  def take(i: Int): Text

  def drop(i: Int): Text

  def splitAt(i: Int): (Text, Text)

  def takeWhile(p: Char => Boolean): Text

  def dropWhile(p: Char => Boolean): Text

  def isPrefixOf(t: Text): Boolean

  def apply(i: Int): Option[Char]

  override def equals(x: Any): Boolean = x.isInstanceOf[Text] && x.toString == toString

  override def hashCode: Int = MurmurHash3.stringHash(toString)
}

object EmptyText extends Text {
  override def empty: Boolean = true

  override def length: Int = 0

  override def toString: String = ""

  override def ++(t: Text): Text = t

  override def head: Option[Char] = Option.empty

  override def tail: Option[Text] = Option.empty

  override def uncons: Option[(Char, Text)] = Option.empty

  override def take(i: Int): Text = EmptyText

  override def drop(i: Int): Text = EmptyText

  override def splitAt(i: Int): (Text, Text) = (EmptyText, EmptyText)

  override def takeWhile(p: Char => Boolean): Text = EmptyText

  override def dropWhile(p: Char => Boolean): Text = EmptyText

  override def isPrefixOf(t: Text): Boolean = true

  override def apply(i: Int): Option[Char] = Option.empty
}

sealed case class NonemptyText private (str: String, offset: Int) extends Text {
  override def empty: Boolean = false

  override def length: Int = str.length() - offset

  override def toString: String = str.substring(offset)

  override def ++(t: Text): Text = Text(toString ++ t.toString)

  override def head: Option[Char] = Option(str.charAt(offset))

  override def tail: Option[Text] = Option(Text(str, offset + 1))

  override def uncons: Option[(Char, Text)] = Option((str.charAt(offset), Text(str, offset + 1)))

  override def take(i: Int): Text = Text(str.substring(offset, math.min(offset + math.max(i, 0), str.length())))

  override def drop(i: Int): Text = Text(str, offset + math.max(i, 0))

  override def splitAt(i: Int): (Text, Text) = (take(i), drop(i))

  override def takeWhile(p: Char => Boolean): Text = Text(str.substring(offset, offset + str.segmentLength(p, offset)))

  override def dropWhile(p: Char => Boolean): Text = Text(str, offset + str.segmentLength(p, offset))

  override def isPrefixOf(t: Text): Boolean = t.toString.regionMatches(0, str, offset, length)

  override def apply(i: Int): Option[Char] = if (i < 0 || i >= length) Option.empty else Option(str.charAt(offset + i))
}

object Text {
  def apply(str: String, index: Int = 0): Text = if (index >= str.length()) EmptyText else NonemptyText(str, math.max(index, 0))

  def escapeChar(c: Char): String = '\'' + (c match {
    case '\b' => "\\b"
    case '\t' => "\\t"
    case '\n' => "\\n"
    case '\f' => "\\f"
    case '\r' => "\\r"
    case '\"' => "\\\""
    case '\'' => "\\\'"
    case '\\' => "\\\\"
    case _ => c.toString
  }) + '\''

  def escapeString(s: String): String = '\"' + s.foldLeft("")((s, c) => c match {
    case '\b' => s + "\\b"
    case '\t' => s + "\\t"
    case '\n' => s + "\\n"
    case '\f' => s + "\\f"
    case '\r' => s + "\\r"
    case '\"' => s + "\\\""
    case '\'' => s + "\\\'"
    case '\\' => s + "\\\\"
    case _ => s + c
  }) + '\"'

  def any[U]: Parser[Text, U, Option[Char]] =
    Parser((s, l, u) => Done(s.uncons
      .map({ case (h, t) => (true, ResultSuccess(t, l.advanceChar(h), u, Option(h))) })
      .getOrElse((true, ResultSuccess(EmptyText, l, u, Option.empty)))))

  def anyChar[U]: Parser[Text, U, Char] =
    Parser((s, l, u) => Done(s.uncons
      .map({ case (h, t) => (true, ResultSuccess(t, l.advanceChar(h), u, h)) })
      .getOrElse((false, ResultError(ParserError(l, List(MessageUnexpected("<EOF>"))))))))

  def eof[U]: Parser[Text, U, Unit] =
    Parser((s, l, u) => Done(s.uncons
      .map({ case (h, t) => (true, ResultError[Text, U, Unit](ParserError(l, List(MessageExpected("<EOF>"))))) })
      .getOrElse((false, ResultSuccess(s, l, u, ())))))

  def char[U](c: Char): Parser[Text, U, Char] =
    Parser((s, l, u) => Done(s.uncons
      .map({ case (h, t) => if (h == c) (true, ResultSuccess(t, l.advanceChar(h), u, h)) else (false, ResultError[Text, U, Char](ParserError(l, List(MessageExpected(escapeChar(c)))))) })
      .getOrElse((false, ResultError(ParserError(l, List(MessageExpected(escapeChar(c)))))))))

  def string[U](str: String): Parser[Text, U, String] =
    Parser((s, l, u) => Done(if (Text(str).isPrefixOf(s))
      (true, ResultSuccess(s.drop(str.length()), l.advanceString(str), u, str))
    else
      (false, ResultError(ParserError(l, List(MessageExpected(escapeString(str))))))))

  def satisfy[U](p: Option[Char] => Boolean): Parser[Text, U, Option[Char]] =
    Parser((s, l, u) => Done(s.uncons
      .map({
        case (h, t) => if (p(Option(h))) (true, ResultSuccess(t, l.advanceChar(h), u, Option(h)))
        else (false, ResultError[Text, U, Option[Char]](ParserError(l, List(MessageUnexpected(escapeChar(h))))))
      })
      .getOrElse(if (p(Option.empty)) (true, ResultSuccess(EmptyText, l, u, Option.empty))
      else (false, ResultError(ParserError(l, List(MessageUnexpected("<EOF>"))))))))

  def dissatisfy[U](p: Option[Char] => Boolean): Parser[Text, U, Option[Char]] = satisfy(!p(_))

  def charSatisfy[U](p: Char => Boolean): Parser[Text, U, Char] =
    Parser((s, l, u) => Done(s.uncons
      .map({
        case (h, t) => if (p(h)) (true, ResultSuccess(t, l.advanceChar(h), u, h))
        else (false, ResultError[Text, U, Char](ParserError(l, List(MessageUnexpected(escapeChar(h))))))
      })
      .getOrElse((false, ResultError(ParserError(l, List(MessageUnexpected("<EOF>"))))))))

  def charDissatisfy[U](p: Char => Boolean): Parser[Text, U, Char] = charSatisfy(!p(_))

  def stringSatisfy[U](p: Char => Boolean): Parser[Text, U, String] =
    Parser((s, l, u) => Done({
      val t = s.takeWhile(p)
      (!t.empty, ResultSuccess(s.drop(t.length), l.advanceString(t.toString), u, t.toString))
    }))

  def stringDissatisfy[U](p: Char => Boolean): Parser[Text, U, String] =
    Parser((s, l, u) => Done(
      (true, {
        val t = s.takeWhile(!p(_))
        ResultSuccess(s.drop(t.length), l.advanceString(t.toString), u, t.toString)
      })))

  def oneOf[U](cs: Option[Char]*): Parser[Text, U, Option[Char]] = satisfy(cs.contains)

  def noneOf[U](cs: Option[Char]*): Parser[Text, U, Option[Char]] = dissatisfy(cs.contains)

  def oneOfChar[U](cs: Char*): Parser[Text, U, Char] = charSatisfy(cs.contains)

  def noneOfChar[U](cs: Char*): Parser[Text, U, Char] = charDissatisfy(cs.contains)
}
