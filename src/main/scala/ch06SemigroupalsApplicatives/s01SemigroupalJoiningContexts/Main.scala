package ch06SemigroupalsApplicatives.s01SemigroupalJoiningContexts

import cats.Semigroupal
import cats.instances.option._
import cats.syntax.apply._


object Main {

  final case class Cat(name: String, born: Int, color: String)

  def main(args: Array[String]): Unit = {
    println(Semigroupal[Option].product(Some(123), Some("abc")))
    println(Semigroupal.map3(Option(1), Option(2), Option(3))(_ + _ + _))

    println((Option(123), Option("xyz")).tupled)

    println((Option("Garfield"), Option(1978), Option("Orange & black")).mapN(Cat.apply))
  }

}
