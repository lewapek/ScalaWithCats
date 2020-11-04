package ch02MonoidsSemigroups.s04AddingThings

import cats.Monoid
import cats.syntax.semigroup._
import cats.instances.int._
import cats.instances.option._
import cats.instances.map._

object Main {
  def add(items: List[Int]): Int = items.foldLeft(0)(_ + _)
  def add[A: Monoid](items: List[A]): A =
    items.foldLeft(Monoid[A].empty)(_ |+| _)

  def main(args: Array[String]): Unit = {
    println(add(((1 to 5)).toList))
    println(add(List(Some(1), None, None, Some(2), Some(4))))

    println(Map("x" -> 6, "y" -> 7) |+| Map("y" -> 1))
  }
}
