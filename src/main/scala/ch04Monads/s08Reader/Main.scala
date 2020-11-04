package ch04Monads.s08Reader

import cats.data.Reader

object Main {

  final case class Cat(name: String, favoriteFood: String)

  def main(args: Array[String]): Unit = {
    val catName: Reader[Cat, String] = Reader(_.name)
    println(catName.run(Cat("Puszek", "sucha karma xd")))

    val greetKitty: Reader[Cat, String] = catName.map(name => s"Hello ${name}")
    println(greetKitty.run(Cat("Heathcliff", "junk food")))

    val feedKitty: Reader[Cat, String] = Reader(cat => s"Have a nice bowl of ${cat.favoriteFood}")
    val greetAndFeed =
      for {
        greet <- greetKitty
        feed <- feedKitty
      } yield s"$greet. $feed."
    println(greetAndFeed.run(Cat("Puszek", "sucha karma xd")))
  }
}
