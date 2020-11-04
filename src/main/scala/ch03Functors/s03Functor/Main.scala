package ch03Functors.s03Functor

import cats.Functor
import cats.instances.option._
import cats.instances.list._
import cats.syntax.functor._

object Main {
  def doMath[F[_]: Functor](start: F[Int]): F[Int] =
    start.map(n => n + 2)

  def main(args: Array[String]): Unit = {
    val list1 = List(1, 2, 3)
    val list2 = Functor[List].map(list1)(_ * 2)
    println(list2)

    val option1 = Option(123)
    val option2 = Functor[Option].map(option1)(_.toString)
    println(option2)

    val f = Functor[Option].lift((x: Int) => x + 2)
    println(f(Some(5)))

    println(Functor[List].as(list1, "x"))

    println(doMath(Option(17)))
    println(doMath(List(17, 18, 19)))
  }

}
