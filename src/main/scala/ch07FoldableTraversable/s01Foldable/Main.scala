package ch07FoldableTraversable.s01Foldable

import cats.Monoid
import cats.instances.int._

object Main {
  def main(args: Array[String]): Unit = {
    val xs = List(1, 2, 3, 4, 5, 6, 7, 8)
    println(map(xs)(_ + 1))

    println(sum2(xs))
  }

  def map[A, B](xs: List[A])(f: A => B): List[B] =
    xs.foldRight(List.empty[B])(f(_) :: _)

  def flatMap[A, B](xs: List[A])(f: A => List[B]): List[B] =
    xs.foldRight(List.empty[B])(f(_) ::: _)

  def filter[A](xs: List[A])(p: A => Boolean): List[A] =
    xs.foldRight(List.empty[A]) {(e, acc) =>
      if (p(e)) e :: acc else acc
    }

  def sum[A: Numeric](xs: List[A]): A = {
    val n = implicitly[Numeric[A]]
    xs.foldRight(n.zero)(n.plus)
  }

  def sum2[A: Monoid](xs: List[A]): A =
    xs.foldRight(Monoid[A].empty)(Monoid[A].combine)

}
