package ch04Monads.s04Either

import cats.syntax.either._

object Main {
  type LoginResult = Either[LoginError, User]

  def handleError(error: LoginError): Unit = error match {
    case UserNotFound(u) =>
      println(s"User not found: $u")
    case PasswordIncorrect(u) =>
      println(s"Password incorrect: $u")
    case UnexpectedError =>
      println(s"Unexpected error")
  }

  def main(args: Array[String]): Unit = {
    val res = {
      for {
        a <- 1.asRight[String]
        b <- 0.asRight[String]
        c <- if (b == 0) Left("div by 0") else (a / b).asRight[String]
      } yield c
    }
    println(res)

    val result1: LoginResult = User("dave", "passw0rd").asRight
    val result2: LoginResult = UserNotFound("dave").asLeft

    result1.fold(handleError, println)
    result2.fold(handleError, println)
  }


}
