package ch03Functors.s05BstFunctor

import cats.syntax.functor._

object Main {
  def main(args: Array[String]): Unit = {
    val t1: Tree[Int] = Branch(Leaf(7), Branch(Leaf(8), Leaf(10)))
    val t2: Tree[Int] = Leaf(1)

    println(t1.map(_ + 2))
    println(t2.map(_ + 2))
  }
}
