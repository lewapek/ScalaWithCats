package ch04Monads.s06Eval

import cats.Eval

import scala.util.Random

object Main {
  def main(args: Array[String]): Unit = {
    val now = Eval.now(Random.nextDouble())
    val always = Eval.always(Random.nextDouble())
    val later = Eval.later(Random.nextDouble())

    println(now.value, always.value, later.value)
    println(now.value, always.value, later.value)

    val greeting = Eval
      .always{ println("Step 1"); "Hello" }
      .map{ str => println("Step 2"); s"$str world" }
    greeting.value

    val ans = for {
      a <- Eval.now{ println("Calculating A"); 40 }
      b <- Eval.always{ println("Calculating B"); 2 }
    } yield {
      println("Adding A and B")
      a + b
    }
    println(ans.value)
    println(ans.value)

    val saying = Eval
      .always{ println("Step 1"); "The cat" }
      .map{ str => println("Step 2"); s"$str sat on" }
      .memoize
      .map{ str => println("Step 3"); s"$str the mat" }

    println(saying.value)
    println(saying.value)
  }

}
