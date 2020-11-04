package ch04Monads.s01Monad

import cats.{Id, Monad}
import cats.instances.option._
import cats.instances.list._
import cats.syntax.flatMap._
import cats.syntax.applicative._
import cats.syntax.functor._
import cats.syntax.either._

object Main {

  def sumSquare[F[_]: Monad](a: F[Int], b: F[Int]): F[Int] = {
    for {
      ae <- a
      be <- b
    } yield ae * ae + be * be
  }

  def main(args: Array[String]): Unit = {
    val opt1 = Monad[Option].pure(3)
    val opt2 = Monad[Option].flatMap(opt1)(n => Some(n + 1))
    val opt3 = Monad[Option].map(opt2)(_ * 2)
    println(opt1, opt2, opt3)

    val list1 = Monad[List].pure(3)
    val list2 = Monad[List].flatMap(List(1, 2, 3))(a => List(a, a * 10))
    val list3 = Monad[List].map(list2)(a => a + 123)
    println(list1, list2, list3)

    println(7.pure[Option])
    println(7.pure[List])

    println(sumSquare[Option](Some(5), Some(8)))
    println(sumSquare(List(1, 2), List(3, 4)))
    println(sumSquare(List(1, 2), List.empty[Int]))
    println(sumSquare(3: Id[Int], 4: Id[Int]))

    println(
      for {
        a <- 6.asRight[String]
        b <- Right[String, Int](8)
      } yield a * b
    )
  }
}
