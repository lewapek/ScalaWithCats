package ch02MonoidsSemigroups.s03BoolMonoid

import BoolMonoidInstances._

object Main {

  def main(args: Array[String]): Unit = {
    val xorRes = for {
      a <- List(true, false)
      b <- List(true, false)
      c <- List(true, false)
    } yield (a, b, c, xorBoolMonoid.combine(xorBoolMonoid.combine(a, b), c), xorBoolMonoid.combine(a, xorBoolMonoid.combine(b, c)))

    xorRes.foreach(println)
  }

}
