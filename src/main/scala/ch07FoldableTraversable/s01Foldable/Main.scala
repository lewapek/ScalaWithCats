package ch07FoldableTraversable.s01Foldable

import cats.{Eval, Foldable, Monoid}
import cats.instances.int._
import cats.instances.list._
import cats.instances.vector._
import cats.instances.string._
import cats.instances.lazyList._

import cats.syntax.foldable._

object Main {

  def bigData: LazyList[Int] = (1 to 100000).to(LazyList)

  def main(args: Array[String]): Unit = {
    val xs = List(1, 2, 3, 4, 5, 6, 7, 8)
    println(map(xs)(_ + 1))
    println(sum2(xs))

    println(Foldable[List].foldLeft(xs, 0)(_ + _))
    val eval: Eval[Long] = Foldable[LazyList].foldRight(bigData, Eval.now(0L)) { (num, eval) =>
      eval.map(_ + num)
    }
    println(eval.value)

    println(Foldable[List].combineAll(List(1, 2, 3)))
    println(Foldable[List].foldMap(xs)(_.toString))

    val ints = List(Vector(1, 2, 3), Vector(4, 5, 6))
    println((Foldable[List] compose Foldable[Vector]).combineAll(ints))

    println(List(1, 2, 3).combineAll)
    println(List(1, 2, 3).foldMap(_.toString))
  }

  def map[A, B](xs: List[A])(f: A => B): List[B] =
    xs.foldRight(List.empty[B])(f(_) :: _)

  def flatMap[A, B](xs: List[A])(f: A => List[B]): List[B] =
    xs.foldRight(List.empty[B])(f(_) ::: _)

  def filter[A](xs: List[A])(p: A => Boolean): List[A] =
    xs.foldRight(List.empty[A]) { (e, acc) =>
      if (p(e)) e :: acc else acc
    }

  def sum[A: Numeric](xs: List[A]): A = {
    val n = implicitly[Numeric[A]]
    xs.foldRight(n.zero)(n.plus)
  }

  def sum2[A: Monoid](xs: List[A]): A =
    xs.foldRight(Monoid[A].empty)(Monoid[A].combine)

}
