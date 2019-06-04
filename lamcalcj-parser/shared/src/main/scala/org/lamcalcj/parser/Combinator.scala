package org.lamcalcj.parser

import org.lamcalcj.parser.Parser._

object Combinator {
  def choice[S, U, A](ps: Parser[S, U, A]*): Parser[S, U, A] = ps.foldRight(mzero[S, U, A])(_ mplus _)

  def option[S, U, A](p: Parser[S, U, A]): Parser[S, U, Option[A]] = p.map(Option.apply) <|> unit(Option.empty)

  def default[S, U, A](a: A, p: Parser[S, U, A]): Parser[S, U, A] = p <|> unit(a)

  def between[S, U, A](open: Parser[S, U, _], close: Parser[S, U, _], p: Parser[S, U, A]): Parser[S, U, A] = for {
    _ <- open
    a <- p
    _ <- close
  } yield a

  def some[S, U, A](p: Parser[S, U, A]): Parser[S, U, List[A]] = recursive(() => many(p)) <|> unit(List.empty)

  def many[S, U, A](p: Parser[S, U, A]): Parser[S, U, List[A]] = for {
    a <- p
    as <- recursive(() => some(p))
  } yield a :: as

  def skipSome[S, U, A](p: Parser[S, U, A]): Parser[S, U, Unit] = recursive(() => skipMany(p)) <|> unit(())

  def skipMany[S, U, A](p: Parser[S, U, A]): Parser[S, U, Unit] = for {
    _ <- p
    _ <- recursive(() => skipSome(p))
  } yield ()

  def someSepBy[S, U, A](p: Parser[S, U, A], sep: Parser[S, U, Unit]): Parser[S, U, List[A]] = manySepBy(p, sep) <|> unit(List.empty)

  def manySepBy[S, U, A](p: Parser[S, U, A], sep: Parser[S, U, Unit]): Parser[S, U, List[A]] = for {
    a <- p
    as <- some(sep >>& p)
  } yield a :: as

  def replicate[S, U, A](i: Int, p: Parser[S, U, A]): Parser[S, U, List[A]] = if (i <= 0) unit(List.empty) else for {
    a <- p
    as <- recursive(() => replicate(i - 1, p))
  } yield a :: as

  def notFollowedBy[S, U, A](p: Parser[S, U, A]): Parser[S, U, Unit] = (for {
    a <- attempt(p)
    r <- mzero[S, U, Unit]
  } yield r) <|> unit(())

  def manyTill[S, U, A](p: Parser[S, U, A], end: Parser[S, U, _]): Parser[S, U, List[A]] = (for {
    _ <- end
  } yield List.empty[A]) <|> (for {
    a <- p
    as <- recursive(() => manyTill(p, end))
  } yield a :: as)
}
