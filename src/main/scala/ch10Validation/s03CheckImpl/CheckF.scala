package ch10Validation.s03CheckImpl

import cats.Semigroup
import cats.syntax.either._
import cats.syntax.semigroup._

final case class CheckF[E: Semigroup, A](func: A => Either[E, A]) {
  def apply(a: A): Either[E, A] = func(a)

  def and(that: CheckF[E, A]): CheckF[E, A] = CheckF { a =>
    (apply(a), that.apply(a)) match {
      case (Right(_), Right(_)) => a.asRight
      case (Right(_), Left(e)) => e.asLeft
      case (Left(e), Right(_)) => e.asLeft
      case (Left(e1), Left(e2)) => (e1 |+| e2).asLeft
    }
  }
}
