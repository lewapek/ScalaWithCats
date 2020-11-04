package ch04Monads.s07WriterMonad

import cats.data.Writer
import cats.syntax.writer._
import cats.instances.vector._
import cats.syntax.applicative._

import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits._
import scala.concurrent.duration._

object MainFactorialExercise {
  def slowly[A](body: => A): A =
    try body finally Thread.sleep(100)

  def factorial(n: Int): Int = {
    val ans = slowly(if (n == 0) 1 else n * factorial(n - 1))
    println(s"fact $n $ans")
    ans
  }

  type Logged[A] = Writer[Vector[String], A]

  def factorialWriter(n: Int): Logged[Int] = slowly {
    for {
      ans <- if (n == 0) 1.pure[Logged] else factorialWriter(n - 1).map(_ * n)
      _ <- Vector(s"fact $n $ans").tell
    } yield ans
  }

  def factorialWriter2(n: Int): Logged[Int] = slowly {
    if (n == 0) 1.pure[Logged]
    else {
      for {
        _ <- Vector(s"n = ${n}").tell
        f <- factorialWriter2(n - 1)
      } yield f * n
    }
  }

  def main(args: Array[String]): Unit = {
    Await.result(Future.sequence(Vector(
      Future(factorial(5)),
      Future(factorial(5)),
    )), 5.seconds)

    val res = Await.result(Future.sequence(Vector(
      Future(factorialWriter(5)),
      Future(factorialWriter(5)),
    )), 5.seconds)
    res.map(_.written).foreach(println)

    val res2 = Await.result(Future.sequence(Vector(
      Future(factorialWriter2(5)),
      Future(factorialWriter2(5)),
    )), 5.seconds)
    res2.map(_.written).foreach(println)
  }


}
