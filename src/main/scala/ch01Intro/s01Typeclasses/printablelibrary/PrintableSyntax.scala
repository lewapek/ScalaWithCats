package ch01Intro.s01Typeclasses.printablelibrary

object PrintableSyntax {
  implicit class PrintableOps[A: Printable](a: A) {
    def format: String = implicitly[Printable[A]].format(a)
    def print(): Unit = println(format)
  }
}
