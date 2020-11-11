package ch10Validation.s03CheckImpl

import cats.data.Validated
import cats.Semigroup
import cats.data.Validated.Invalid
import cats.syntax.apply._
import cats.syntax.semigroup._
import cats.syntax.validated._

trait CheckValidated[E, A] {
  import CheckValidated._

  def and(that: CheckValidated[E, A]): CheckValidated[E, A] = And(this, that)

  def or(that: CheckValidated[E, A]): CheckValidated[E, A] = Or(this, that)

  def apply(a: A)(implicit s: Semigroup[E]): Validated[E, A] = this match {
    case Pure(f) => f(a)
    case And(left, right) => (left(a), right(a)).mapN((_, _) => a)
    case Or(left, right) => (left(a), right(a)) match {
      case (Invalid(e1), Invalid(e2)) => (e1 |+| e2).invalid
      case _ => a.valid
    }
  }

}

object CheckValidated {
  final case class And[E, A](left: CheckValidated[E, A], right: CheckValidated[E, A]) extends CheckValidated[E, A]
  final case class Or[E, A](left: CheckValidated[E, A], right: CheckValidated[E, A]) extends CheckValidated[E, A]
  final case class Pure[E, A](f: A => Validated[E, A]) extends CheckValidated[E, A]

  def pure[E, A](f: A => Validated[E, A]): CheckValidated[E, A] = Pure(f)
}