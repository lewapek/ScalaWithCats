package ch04Monads.s08Reader

import cats.data.Reader
import cats.instances.boolean._
import cats.syntax.applicative._

final case class Db(usernames: Map[Int, String],
                    passwords: Map[String, String])

object MainExercise {

  type DbReader[A] = Reader[Db, A]

  def main(args: Array[String]): Unit = {
    val users = Map(
      1 -> "dade",
      2 -> "kate",
      3 -> "margo"
    )
    val passwords = Map(
      "dade"  -> "zerocool",
      "kate"  -> "acidburn",
      "margo" -> "secret"
    )
    val db = Db(users, passwords)
    println(checkLogin(1, "zerocool").run(db))
    println(checkLogin(3, "zerocool").run(db))
    println(checkLogin(4, "asd").run(db))
  }

  def findUsername(userId: Int): DbReader[Option[String]] =
    Reader(_.usernames.get(userId))

  def checkPassword(username: String, password: String): DbReader[Boolean] =
    Reader(_.passwords.get(username).contains(password))

  def checkLogin(userId: Int, password: String): DbReader[Boolean] =
    for {
      username <- findUsername(userId)
      checked <- username.map(checkPassword(_, password)).getOrElse(false.pure[DbReader])
    } yield checked
}
