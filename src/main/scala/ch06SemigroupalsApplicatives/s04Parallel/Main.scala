package ch06SemigroupalsApplicatives.s04Parallel

import cats.Semigroupal
import cats.instances.either._
import cats.syntax.apply._
import cats.syntax.parallel._
import cats.instances.vector._
import cats.instances.list._

object Main {

  type ErrorOr[A] = Either[Vector[String], A]

  def main(args: Array[String]): Unit = {
    val error1: ErrorOr[Int] = Left(Vector("Error 1"))
    val error2: ErrorOr[Int] = Left(Vector("Error 2"))

    println(Semigroupal[ErrorOr].product(error1, error2))
    println((error1, error2).tupled)
    println((error1, error2).parTupled)


    val success1: ErrorOr[Int] = Right(1)
    val success2: ErrorOr[Int] = Right(2)
    val addTwo = (x: Int, y: Int) => x + y
    println((error1, error2).parMapN(addTwo))
    println((success1, success2).parMapN(addTwo))

    println((List(1, 2), List(3, 4)).tupled)
    println((List(1, 2), List(3, 4)).parTupled)
  }

}
