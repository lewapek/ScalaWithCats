package ch03Functors.s06CatsContravariant

import cats.{Monoid, Show}
import cats.syntax.contravariant._
import cats.instances.string._
import cats.syntax.invariant._
import cats.syntax.semigroup._
import cats.syntax.show._

object Main {
  implicit val symbolMonoid: Monoid[Symbol] = Monoid[String].imap[Symbol](Symbol.apply)(_.name)
  implicit val showSymbol: Show[Symbol] = Show[String].contramap[Symbol]("'" + _.name)

  def main(args: Array[String]): Unit = {
    println(showSymbol.show(Symbol("asd")))

    println(Monoid[Symbol].empty.show)
    println((Symbol("Sc") |+| Symbol("al") |+| Symbol("a!")).show)
  }

}
