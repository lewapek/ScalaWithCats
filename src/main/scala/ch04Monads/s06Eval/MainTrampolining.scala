package ch04Monads.s06Eval

import cats.Eval

object MainTrampolining {
  // using Eval which is trampolined - doesn't consume stack frames
  // however recursive call to factorial is done before .map which is trampolined
  def factorial(n: BigInt): Eval[BigInt] =
    if (n <= 1) Eval.now(1) else factorial(n - 1).map(_ * n)

  // defer is trampolined
  def factorialDefer(n: BigInt): Eval[BigInt] =
    if (n <= 1) Eval.now(1) else Eval.defer(factorialDefer(n - 1).map(_ * n))

  def main(args: Array[String]): Unit = {
    //    println(factorial(50000).value) // stack overflow
    println(factorialDefer(50000).value)

    //    println(foldRight((1 to 100000).toList, 0)(_ + _) // stack overflow)
    println(foldRightRedefined((1 to 100000).toList, 0L)(_ + _))
  }

  def foldRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): B = as match {
    case head :: tail =>
      fn(head, foldRight(tail, acc)(fn))
    case Nil =>
      acc
  }

  def foldRightEval[A, B](as: List[A], acc: Eval[B])(fn: (A, Eval[B]) => Eval[B]): Eval[B] = as match {
    case head :: tail =>
      val deferred = Eval.defer(foldRightEval(tail, acc)(fn))
      fn(head, deferred)
    case Nil =>
      acc
  }

  def foldRightRedefined[A, B](as: List[A], acc: B)(fn: (A, B) => B): B =
    foldRightEval(as, Eval.now(acc))((a, b) => b.map(fn(a, _))).value


}
