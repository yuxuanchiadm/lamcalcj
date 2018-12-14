package org.lamcalcj.utils

object Trampoline {
  sealed trait Trampoline[+A] {
    final def resume: Either[() => Trampoline[A], A] = this match {
      case Done(v) => Right(v)
      case More(k) => Left(k)
      case FlatMap(a, f) => a match {
        case Done(v) => f(v).resume
        case More(k) => Left(() => k() flatMap f)
        case FlatMap(b, g) => b.flatMap((x: Any) => g(x) flatMap f).resume
      }
    }

    final def runT: A = resume match {
      case Right(a) => a
      case Left(k) => k().runT
    }

    def flatMap[B](f: A => Trampoline[B]): Trampoline[B] = this match {
      case FlatMap(a, g) => FlatMap(a, (x: Any) => g(x) flatMap f)
      case x => FlatMap(x, f)
    }

    def map[B](f: A => B): Trampoline[B] =
      flatMap(a => Done(f(a)))
  }
  case class Done[+A](a: A) extends Trampoline[A]
  case class More[+A](k: () => Trampoline[A]) extends Trampoline[A]
  case class FlatMap[A, +B](sub: Trampoline[A], k: A => Trampoline[B]) extends Trampoline[B]

}
