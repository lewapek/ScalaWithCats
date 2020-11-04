package ch01Intro.s04Show

import java.util.Date

import cats.Show
import cats.instances.int.catsStdShowForInt
import cats.instances.string.catsStdShowForString
import cats.syntax.show._

object Main {

  val showInt = Show.apply[Int]
  val showString = Show.apply[String]

  implicit val dateShow: Show[Date] = Show.show(d => s"${d.getTime}ms since the epoch.")

  def main(args: Array[String]): Unit = {
    println(showInt.show(123))
    println(showString.show("asd"))

    println(123.show)
    println("asd".show)
    println(new Date().show)

  }

}
