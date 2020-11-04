package ch03Functors.s07PartialUnificationContravariantSwapParams

import cats.syntax.show._
import cats.instances.double._
import cats.syntax.contravariant._
import cats.instances.function._

object Main {
  val f1: Int => Double = _.toDouble + 1.0
  val f2: Double => String = d => (d * 2.5).show

  def main(args: Array[String]): Unit = {
    val g1: Int => String = i => f2(f1(i))
    val g2: Int => String = f2 compose f1
    //    val g3: Int => String = f2 contramap f1 // doesn't compile
    type <=[B, A] = A => B
    val f2b: String <= Double = f2
    val g3b = f2b contramap f1 // partial unification works properly now, similar trick to haskell newtype

    println(g1(5))
    println(g2(5))
    println(g3b(5))
  }

}
