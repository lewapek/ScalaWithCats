package ch10Validation.s03CheckImpl

import cats.instances.list._
import cats.syntax.either._
import cats.syntax.validated._

object Main {

  def checkFExample(): Unit = {
    val a: CheckF[List[String], Int] = CheckF { int =>
      if (int > 2) int.asRight else List(s"$int must be > 2").asLeft
    }
    val b: CheckF[List[String], Int] = CheckF { int =>
      if (int < -2) int.asRight else List(s"$int must be < -2").asLeft
    }

    val check = a and b
    println(check(5))
    println(check(-1))
  }

  def checkADTExample(): Unit = {
    val a: Check[List[String], Int] = Check.pure { int =>
      if (int > 2) int.asRight else List(s"$int must be > 2").asLeft
    }
    val b: Check[List[String], Int] = Check.pure { int =>
      if (int < -2) int.asRight else List(s"$int must be < -2").asLeft
    }

    val check: Check[List[String], Int] =  a and b
    println(check(5))
    println(check(-1))
  }

  def checkADTValidatedExample(): Unit = {
    val a: CheckValidated[List[String], Int] = CheckValidated.pure { int =>
      if (int > 2) int.valid else List(s"$int must be > 2").invalid
    }
    val b: CheckValidated[List[String], Int] = CheckValidated.pure { int =>
      if (int < -2) int.valid else List(s"$int must be < -2").invalid
    }

    val check: CheckValidated[List[String], Int] =  a and b
    println(check(5))
    println(check(-1))
  }

  def main(args: Array[String]): Unit = {
    checkFExample()
    checkADTExample()
    checkADTValidatedExample()
  }

}
