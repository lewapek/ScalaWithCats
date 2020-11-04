package ch04Monads.s07WriterMonad

import cats.Id
import cats.data.{Writer, WriterT}
import cats.instances.vector._
import cats.syntax.applicative._
import cats.syntax.writer._

object Main {

  type Writer[W, A] = WriterT[Id, W, A]
  type Logged[A] = Writer[Vector[String], A]

  def main(args: Array[String]): Unit = {
    println(Writer(
      Vector("It was the best of times", "it was the worst of times"),
      1859)
    )
    println(123.pure[Logged])
    println(Vector("asd", "qwe").tell)
    println(Writer(Vector("msg1", "msg2", "msg3"), 123))
    val w = 123.writer(Vector("msg1", "msg2", "msg3"))
    println(w)
    println(w.value)
    println(w.written)
    println(w.run)

    val writer1 = for {
      a <- 10.pure[Logged]
      _ <- Vector("a", "b", "c").tell
      b <- 32.writer(Vector("x", "y", "z"))
    } yield a + b
    println(writer1.run)

    println(writer1.mapWritten(_.map(_ + "!")).run)
    println(writer1.bimap(_.map(_ + "!"), _ * 2).run)
    println(writer1.mapBoth((w, a) => (w.map(_ + "!"), a * 2)).run)

    println(writer1.reset.run)
    println(writer1.swap.run)

  }

}
