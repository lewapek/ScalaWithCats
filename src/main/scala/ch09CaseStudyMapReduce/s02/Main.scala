package ch09CaseStudyMapReduce.s02

import cats.Monoid
import cats.instances.future._
import cats.instances.int._
import cats.instances.string._
import cats.instances.vector._
import cats.syntax.foldable._
import cats.syntax.monoid._
import cats.syntax.traverse._

import scala.concurrent.ExecutionContext.Implicits
import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}

object Main {

  def foldMap[A, B: Monoid](seq: Vector[A])(f: A => B): B =
    seq.foldLeft(Monoid[B].empty)(_ |+| f(_))

  def parallelFoldMap[A, B: Monoid](seq: Vector[A])
                                   (f: A => B)
                                   (implicit ec: ExecutionContext): Future[B] = {
    val cores = Runtime.getRuntime.availableProcessors()
    val groupSize = (seq.length.toFloat / cores).ceil.toInt
    val groups = seq.grouped(groupSize)

    val futures = groups.map(chunk => Future(foldMap(chunk)(f)))
    futures.toVector.sequence.map(Monoid[B].combineAll)
  }

  def parallelFoldMapCats[A, B: Monoid](seq: Vector[A])
                                       (f: A => B)
                                       (implicit ec: ExecutionContext): Future[B] = {
    val cores = Runtime.getRuntime.availableProcessors()
    val groupSize = (seq.length.toFloat / cores).ceil.toInt

    seq
      .grouped(groupSize)
      .toVector
      .traverse(chunk => Future(chunk.foldMap(f)))
      .map(_.combineAll)
  }

  def main(args: Array[String]): Unit = {
    implicit val ec: ExecutionContext = Implicits.global

    println(foldMap(Vector(1, 2, 3))(identity))
    println(foldMap(Vector(1, 2, 3))(_.toString + "->"))

    println(Await.result(
      parallelFoldMap((1 to 1000000).toVector)(identity[Int]),
      1 second
    ))
    println(Await.result(
      parallelFoldMapCats((1 to 1000000).toVector)(identity[Int]),
      1 second
    ))
  }

}
