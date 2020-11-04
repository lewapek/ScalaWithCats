package ch01Intro.s01Typeclasses.printablelibrary

import PrintableInstances._
import PrintableSyntax._

final case class Cat(name: String, age: Int, color: String)
object Cat {
  import Printable._
  implicit val printableCat: Printable[Cat] =
    c => s"${format(c.name)} is a ${format(c.age)} years old ${format(c.color)} cat."
}

object Main {
  def main(args: Array[String]): Unit = {
    val cat = Cat("Szaruś", 9, "grey")
    cat.print()
  }
}
