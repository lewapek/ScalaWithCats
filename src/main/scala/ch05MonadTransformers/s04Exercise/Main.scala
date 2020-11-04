package ch05MonadTransformers.s04Exercise

import cats.data.EitherT

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import cats.instances.future._
import cats.syntax.monad._

import scala.concurrent.duration.DurationInt
import scala.language.postfixOps

object Main {

  type Response[A] = EitherT[Future, String, A]

  val powerLevels = Map(
    "Jazz" -> 6,
    "Bumblebee" -> 8,
    "Hot Rod" -> 10
  )

  def main(args: Array[String]): Unit = {
    println(tacticalReport("Jazz", "Bumblebee"))
    println(tacticalReport("Bumblebee", "Hot Rod"))
    println(tacticalReport("Jazz", "Ironhide"))
  }

  def getPowerLevel(autobot: String): Response[Int] = {
    powerLevels.get(autobot) match {
      case Some(value) => EitherT.right(Future.successful(value))
      case None => EitherT.left(Future.successful(s"$autobot not found"))
    }
  }

  def canSpecialMove(ally1: String, ally2: String): Response[Boolean] = {
    for {
      level1 <- getPowerLevel(ally1)
      level2 <- getPowerLevel(ally2)
    } yield level1 + level2 > 15
  }

  def tacticalReport(ally1: String, ally2: String): String = {
    val result = canSpecialMove(ally1, ally2).value

    Await.result(result, 1 second) match {
      case Left(value) => s"error: $value"
      case Right(false) => "too low power"
      case Right(true) => "enough power"
    }
  }
}
