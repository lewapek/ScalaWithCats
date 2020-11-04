package ch05MonadTransformers.s03OptionT

import cats.data.{EitherT, OptionT}
import cats.instances.either._
import cats.instances.future._
import cats.syntax.applicative._
import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._


import scala.concurrent.Future

object Main {

  type ListOption[A] = OptionT[List, A]

  type ErrorOr[A] = Either[String, A]
  type ErrorOrOption[A] = OptionT[ErrorOr, A]

  type ErrorOrF[A] = EitherT[Future, String, A]
  type FutureEitherOption[A] = OptionT[ErrorOrF, A]

  type FEO[A] = OptionT[EitherT[Future, String, *], A] // kind projector

  def main(args: Array[String]): Unit = {
    val a = 10.pure[ErrorOrOption]
    val b = 17.pure[ErrorOrOption]
    val c =
      for {
        aa <- a
        bb <- b
      } yield aa + bb
    println(c)

    val futureEitherOption: FutureEitherOption[Int] =
      for {
        a <- 10.pure[FutureEitherOption]
        b <- 32.pure[FutureEitherOption]
      } yield a + b
    println(futureEitherOption)
  }

}
