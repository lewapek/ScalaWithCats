package ch04Monads.s10CustomMonads

import cats.syntax.functor._
import cats.syntax.flatMap._
import Tree._

object MainTree {
  def main(args: Array[String]): Unit = {
    val transformed = branch(leaf(100), leaf(200)).flatMap { x =>
      branch(leaf(x - 1), leaf(x + 1))
    }
    println(transformed)

    val transformed2 = {
      for {
        a <- branch(leaf(100), leaf(200))
        b <- branch(leaf(a - 10), leaf(a + 10))
        c <- branch(leaf(b - 1), leaf(b + 1))
      } yield c
    }
    println(transformed2)
  }

}
