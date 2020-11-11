package ch10Validation.s03CheckImpl

import cats.Semigroup
import cats.syntax.either._
import cats.syntax.semigroup._

sealed trait Check[E, A] {
  import Check._

  def and(that: Check[E, A]): Check[E, A] = And(this, that)

  def apply(a: A)(implicit s: Semigroup[E]): Either[E, A] = this match {
    case Pure(f) => f(a)
    case And(left, right) =>
      (left(a), right(a)) match {
        case (Left(e1), Left(e2)) => (e1 |+| e2).asLeft
        case (Left(e), Right(_)) => e.asLeft
        case (Right(_), Left(e)) => e.asLeft
        case _ => a.asRight
      }
  }

}

object Check {
  final case class And[E, A](left: Check[E, A], right: Check[E, A]) extends Check[E, A]
  final case class Pure[E, A](f: A => Either[E, A]) extends Check[E, A]

  def pure[E, A](f: A => Either[E, A]): Check[E, A] = Pure(f)
}
