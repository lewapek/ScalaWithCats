package ch01Intro.s01Typeclasses.printablelibrary

object PrintableInstances {
  implicit val intPrintable: Printable[Int] = String.valueOf(_)
  implicit val stringPrintable: Printable[String] = identity

  implicit def optionPrintable[A](implicit printable: Printable[A]): Printable[Option[A]] = {
    case Some(a) => printable.format(a)
    case None => ""
  }

}
