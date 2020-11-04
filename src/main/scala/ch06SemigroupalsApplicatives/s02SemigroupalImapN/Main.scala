package ch06SemigroupalsApplicatives.s02SemigroupalImapN

import cats.{Monoid, Semigroupal}
import cats.instances.string._
import cats.instances.int._
import cats.instances.list._
import cats.syntax.apply._
import cats.instances.invariant._
import cats.syntax.semigroup._

object Main {

  final case class Cat(name: String, yearOfBirth: Int, favoriteFoods: List[String])

  val tupleToCat: (String, Int, List[String]) => Cat =
    Cat.apply

  val catToTuple: Cat => (String, Int, List[String]) =
    cat => (cat.name, cat.yearOfBirth, cat.favoriteFoods)

  implicit val catMonoid: Monoid[Cat] = (
    Monoid[String],
    Monoid[Int],
    Monoid[List[String]]
  ).imapN(tupleToCat)(catToTuple)

  def main(args: Array[String]): Unit = {
    val garfield   = Cat("Garfield", 1978, List("Lasagne"))
    val heathcliff = Cat("Heathcliff", 1988, List("Junk Food"))

    println(garfield |+| heathcliff)


    println(Semigroupal[List].product(List(1, 2), List(3, 4)))
  }

}
