package ch10Validation.s04PredicateCheck

import cats.data.Validated
import cats.Semigroup
import cats.data.Validated.Invalid
import cats.syntax.apply._
import cats.syntax.semigroup._
import cats.syntax.validated._

trait Predicate[E, A] {
  import Predicate._

  def and(that: Predicate[E, A]): Predicate[E, A] = And(this, that)

  def or(that: Predicate[E, A]): Predicate[E, A] = Or(this, that)

  def apply(a: A)(implicit s: Semigroup[E]): Validated[E, A] = this match {
    case Pure(f) => f(a)
    case And(left, right) => (left(a), right(a)).mapN((_, _) => a)
    case Or(left, right) => (left(a), right(a)) match {
      case (Invalid(e1), Invalid(e2)) => (e1 |+| e2).invalid
      case _ => a.valid
    }
  }

}

object Predicate {
  final case class And[E, A](left: Predicate[E, A], right: Predicate[E, A]) extends Predicate[E, A]
  final case class Or[E, A](left: Predicate[E, A], right: Predicate[E, A]) extends Predicate[E, A]
  final case class Pure[E, A](f: A => Validated[E, A]) extends Predicate[E, A]

  def pure[E, A](f: A => Validated[E, A]): Predicate[E, A] = Pure(f)
}
