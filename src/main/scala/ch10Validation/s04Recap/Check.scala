package ch10Validation.s04Recap

import cats.Semigroup
import cats.data.Validated

sealed trait Check[E, A, B] {

  import Check._

  def apply(in: A)(implicit s: Semigroup[E]): Validated[E, B]

  def map[C](f: B => C): Check[E, A, C] =
    Map[E, A, B, C](this, f)

  def flatMap[C](f: B => Check[E, A, C]): Check[E, A, C] =
    FlatMap[E, A, B, C](this, f)

  def andThen[C](next: Check[E, B, C]): Check[E, A, C] =
    AndThen[E, A, B, C](this, next)
}

object Check {

  final case class Map[E, A, B, C](check: Check[E, A, B], f: B => C) extends Check[E, A, C] {
    def apply(a: A)(implicit s: Semigroup[E]): Validated[E, C] =
      check(a) map f
  }

  final case class FlatMap[E, A, B, C](check: Check[E, A, B], f: B => Check[E, A, C]) extends Check[E, A, C] {
    def apply(a: A)(implicit s: Semigroup[E]): Validated[E, C] =
      check(a).withEither(_.flatMap(b => f(b)(a).toEither))
  }

  final case class AndThen[E, A, B, C](check: Check[E, A, B], next: Check[E, B, C]) extends Check[E, A, C] {
    def apply(a: A)(implicit s: Semigroup[E]): Validated[E, C] =
      check(a).withEither(_.flatMap(b => next(b).toEither))
  }

  final case class Pure[E, A, B](func: A => Validated[E, B]) extends Check[E, A, B] {
    def apply(a: A)(implicit s: Semigroup[E]): Validated[E, B] =
      func(a)
  }

  final case class PurePredicate[E, A](p: Predicate[E, A]) extends Check[E, A, A] {
    def apply(a: A)(implicit s: Semigroup[E]): Validated[E, A] =
      p(a)
  }

  def apply[E, A](p: Predicate[E, A]): Check[E, A, A] =
    PurePredicate(p)

  def apply[E, A, B](f: A => Validated[E, B]): Check[E, A, B] =
    Pure(f)
}

