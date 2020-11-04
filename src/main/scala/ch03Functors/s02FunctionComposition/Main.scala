package ch03Functors.s02FunctionComposition

import cats.syntax.functor._
import cats.instances.function._

object Main {
  val f1: Int => Double = _.toDouble
  val f2: Double => Double = _ * 2

  def main(args: Array[String]): Unit = {
    val functions: List[Int => Double] = List(
      f1 map f2,
      f1 andThen f2,
      (x: Int) => f2(f1(x))
    )

    functions.foreach { f =>
      println(f(1))
    }

  }
}
