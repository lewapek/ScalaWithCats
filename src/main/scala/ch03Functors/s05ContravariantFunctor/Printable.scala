package ch03Functors.s05ContravariantFunctor

trait Printable[A] { self =>

  def format(value: A): String

  def contramap[B](f: B => A): Printable[B] = (value: B) => self.format(f(value))
}

object Printable {
  def format[A: Printable](value: A): String = implicitly[Printable[A]].format(value)

  implicit val stringPrintable: Printable[String] = s => s"'$s'"
  implicit val boolPrintable: Printable[Boolean] = if (_) "yes" else "no"

//  implicit def boxPrintable[A: Printable]: Printable[Box[A]] = box => implicitly[Printable[A]].format(box.a)
  implicit def boxPrintable[A: Printable]: Printable[Box[A]] = implicitly[Printable[A]].contramap(_.a)
}

case class Box[A](a: A)