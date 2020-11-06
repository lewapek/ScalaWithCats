package ch07FoldableTraversable.s02Traverse

import cats.{Applicative, Traverse}
import cats.data.Validated
import cats.syntax.applicative._
import cats.syntax.traverse._
import cats.syntax.apply._
import cats.instances.future._
import cats.instances.vector._
import cats.instances.option._
import cats.instances.list._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.language.postfixOps
import scala.concurrent.duration._

object Main {

  def listTraverse[F[_] : Applicative, A, B]
  (list: List[A])(func: A => F[B]): F[List[B]] =
    list.foldLeft(List.empty[B].pure[F]) { (accum, item) =>
      (accum, func(item)).mapN(_ :+ _)
    }

  def listSequence[F[_] : Applicative, B]
  (list: List[F[B]]): F[List[B]] =
    listTraverse(list)(identity)

  def process(inputs: List[Int]): Option[List[Int]] =
    listTraverse(inputs)(n => if (n % 2 == 0) Some(n) else None)

  type ErrorsOr[A] = Validated[List[String], A]

  def processValidated(inputs: List[Int]): ErrorsOr[List[Int]] =
    listTraverse(inputs) { n =>
      if (n % 2 == 0) Validated.valid(n)
      else            Validated.invalid(List(s"$n is not even"))
    }

  def main(args: Array[String]): Unit = {
    val xs = List(1, 2, 3)

    val res = listTraverse(xs)(Future.successful)
    println(Await.result(res, 1 second))

    println(listSequence(List(Vector(1, 2), Vector(3, 4), Vector(5, 6))))

    println(process(List(2, 4, 6)))
    println(process(List(1, 2, 3)))

    println(processValidated(List(2, 4, 6)))
    println(processValidated(List(1, 2, 3)))

    println(Traverse[List].traverse(xs)(Option.apply))
    println(xs.traverse(Option.apply))
    println(List(Future.successful(1), Future.successful(2)).sequence)
  }

}
