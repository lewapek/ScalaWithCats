package ch01Intro.s05Eq

import java.util.Date
import java.util.concurrent.TimeUnit

import cats.Eq
import cats.instances.int._
import cats.instances.string._
import cats.instances.option._
import cats.syntax.eq._
import cats.syntax.option._

object Main {
  val intEq = Eq[Int]

  implicit val dateEq: Eq[Date] = Eq.instance(_.getTime == _.getTime)

  case class Person(name: String, age: Int)
  implicit val personEq: Eq[Person] = Eq.instance((p1, p2) => p1.name === p2.name && p1.age === p2.age)

  def main(args: Array[String]): Unit = {
    println(intEq.eqv(123, 123))
    println(123 === 123)
    println(123 =!= 123)

    println(1.some === None)

    val x = new Date()
    TimeUnit.MILLISECONDS.sleep(5)
    val y = new Date()
    println(x === y)

    println(Person("asd", 12) =!= Person("asd", 13))
    println(Person("asd", 12).some =!= Option.empty[Person])
  }
}
