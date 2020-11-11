package ch10Validation.s04PredicateCheck

import cats.Semigroup
import cats.data.Validated
import cats.syntax.either._

sealed trait Check[E, A, B] {

  import Check._

  def apply(a: A)(implicit s: Semigroup[E]): Validated[E, B]

  def andThen[C](that: Check[E, B, C]): Check[E, A, C] =
    AndThen(this, that)

  def map[C](f: B => C): Check[E, A, C] =
    Map[E, A, B, C](this, f)

  def flatMap[C](f: B => Check[E, A, C]): Check[E, A, C] =
    FlatMap[E, A, B, C](this, f)

}

object Check {

  final case class Map[E, A, B, C](check: Check[E, A, B], func: B => C) extends Check[E, A, C] {
    def apply(in: A)(implicit s: Semigroup[E]): Validated[E, C] =
      check(in).map(func)
  }

  final case class FlatMap[E, A, B, C](check: Check[E, A, B], func: B => Check[E, A, C]) extends Check[E, A, C] {
    def apply(a: A)(implicit s: Semigroup[E]): Validated[E, C] =
      check(a).withEither(_.flatMap(b => func(b)(a).toEither))
  }

  final case class AndThen[E, A, B, C](c1: Check[E, A, B], c2: Check[E, B, C]) extends Check[E, A, C] {
    def apply(a: A)(implicit s: Semigroup[E]): Validated[E, C] =
      c1(a).withEither(
        _.flatMap(b => c2(b).toEither)
      )
  }

  final case class Pure[E, A](p: Predicate[E, A]) extends Check[E, A, A] {
    def apply(in: A)(implicit s: Semigroup[E]): Validated[E, A] =
      p(in)
  }

  def apply[E, A](p: Predicate[E, A]): Check[E, A, A] =
    Pure(p)
}