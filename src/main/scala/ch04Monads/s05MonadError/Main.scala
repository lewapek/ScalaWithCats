package ch04Monads.s05MonadError

import scala.util.Try

import cats.MonadError
import cats.instances.either._

import cats.syntax.applicative._
import cats.syntax.applicativeError._
import cats.syntax.monadError._
import cats.instances.try_._

object Main {
  type ErrorOr[A] = Either[String, A]
  val monadError: MonadError[ErrorOr, String] = MonadError[ErrorOr, String]

  def main(args: Array[String]): Unit = {
    val success = monadError.pure(123)
    val failure = monadError.raiseError[Int]("fail")

    val handled = monadError.handleErrorWith(failure) {
      case "fail" => monadError.pure(0)
      case _ => monadError.raiseError("really bad!")
    }

    println(success)
    println(failure)
    println(handled)
    println(monadError.ensure(success)("number too low")(_ > 1000))
    println(monadError.ensure(failure)("number too low")(_ > 1000))

    val succ = 42.pure[ErrorOr]
    val fail = "Badness".raiseError[ErrorOr, Int]
    val handlerFail = fail.handleErrorWith {
      case "Badness" =>
        256.pure[ErrorOr]
      case _ =>
        "It's not ok".raiseError[ErrorOr, Int]
    }
    println(handlerFail)
    println(succ.ensure("err")(_ > 1000))

    val throwable = new RuntimeException
    println(throwable.raiseError[Try, Int])

    println(validateAdult[Try](18))
    println(validateAdult[Try](11))
    type ThrowableOr[A] = Either[Throwable, A]
    println(validateAdult[ThrowableOr](19))
  }

  def validateAdult[F[_]](age: Int)(implicit me: MonadError[F, Throwable]): F[Int] =
    if (age >= 18) age.pure[F] else (new IllegalArgumentException).raiseError[F, Int]


}
