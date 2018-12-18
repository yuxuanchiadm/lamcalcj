package org.lamcalcj.utils

import org.lamcalcj.utils.Trampoline._

object Except {
  sealed case class Except[E, A](runExceptT: Trampoline[Either[E, A]]) {
    final def runExcept: Either[E, A] = runExceptT.runT

    final def flatMap[B](f: A => Except[E, B]): Except[E, B] =
      Except(runExceptT.flatMap(v => v match {
        case Left(e) => Done(Left(e))
        case Right(a) => More(() => f(a).runExceptT)
      }))

    final def map[B](f: A => B): Except[E, B] = flatMap(a => unit(f(a)))

    final def catchE[F](f: E => Except[F, A]): Except[F, A] =
      Except(runExceptT.flatMap(v => v match {
        case Left(e) => More(() => f(e).runExceptT)
        case Right(a) => Done(Right(a))
      }))

    final def withExcept[F](f: E => F): Except[F, A] = catchE(e => throwE(f(e)))
  }

  final def unit[E, A](a: A): Except[E, A] = Except(Done(Right(a)))

  final def except[E, A](v: Either[E, A]): Except[E, A] = Except(Done(v))

  final def throwE[E, A](e: E): Except[E, A] = Except(Done(Left(e)))

  final def recursive[E, A](k: () => Except[E, A]): Except[E, A] = Except(More(() => k().runExceptT))
}