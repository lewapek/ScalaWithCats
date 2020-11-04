package ch04Monads.s10CustomMonads

import cats.Monad

import scala.annotation.tailrec
import cats.syntax.flatMap._
import cats.instances.option._
import cats.syntax.functor._
import cats.syntax.either._
import cats.syntax.monad._

object Main {
  def main(args: Array[String]): Unit = {
    println(retry(100)(a => if (a == 0) None else Some(a - 1)))
    println(retryTailRecM(100000)(a => if (a == 0) None else Some(a - 1)))
  }

  def retry[F[_] : Monad, A](start: A)(f: A => F[A]): F[A] =
    f(start).flatMap(retry(_)(f)) // not stack sate

  def retryTailRecM[F[_] : Monad, A](start: A)(f: A => F[A]): F[A] =
    Monad[F].tailRecM(start) {
      f(_).map(_.asLeft[A])
    }

  def retryM[F[_]: Monad, A](start: A)(f: A => F[A]): F[A] =
    start.iterateWhileM(f)(_ => true)

  val optionMonad: Monad[Option] = new Monad[Option] {
    def flatMap[A, B](opt: Option[A])(fn: A => Option[B]): Option[B] = opt flatMap fn

    def pure[A](opt: A): Option[A] = Some(opt)

    @tailrec
    def tailRecM[A, B](a: A)(fn: A => Option[Either[A, B]]): Option[B] = fn(a) match {
      case None => None
      case Some(Left(a1)) => tailRecM(a1)(fn)
      case Some(Right(b)) => Some(b)
    }
  }

}
