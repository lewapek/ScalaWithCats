package ch04Monads.s09State

import cats.data.State
import cats.syntax.applicative._

object MainRPN {

  type CalcState[A] = State[List[Int], A]

  def main(args: Array[String]): Unit = {
    val program = for {
      _ <- evalOne("1")
      _ <- evalOne("2")
      ans <- evalOne("+")
    } yield ans
    println(program.run(Nil).value)

    val multistageProgram = evalAll(List("1", "2", "+", "3", "*"))
    println(multistageProgram.run(Nil).value)

    val biggerProgram = for {
      _ <- evalAll(List("1", "2", "+"))
      _ <- evalAll(List("3", "4", "+"))
      ans <- evalOne("*")
    } yield ans
    println(biggerProgram.run(Nil).value)

    println(evalInput("1 2 + 3 4 + *"))
  }

  def evalInput(input: String): Int =
    evalAll(input.split(" ").toList).runA(Nil).value

  def evalAll(input: List[String]): CalcState[Int] = {
    input.foldLeft(0.pure[CalcState]) { (state, string) =>
      state.flatMap(_ => evalOne(string))
    }
  }

  def evalOne(sym: String): CalcState[Int] = sym match {
    case "+" => operator(_ + _)
    case "-" => operator(_ - _)
    case "*" => operator(_ * _)
    case num => State(s => (num.toInt :: s, num.toInt))
  }

  def operator(f: (Int, Int) => Int): CalcState[Int] = State {
    case a :: b :: tail =>
      val res = f(a, b)
      (res :: tail, res)
    case _ => sys.error("fail")
  }

}
