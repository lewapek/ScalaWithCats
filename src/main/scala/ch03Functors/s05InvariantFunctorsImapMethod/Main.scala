package ch03Functors.s05InvariantFunctorsImapMethod

object Main {
  def main(args: Array[String]): Unit = {
    println(Codec.encode(123))
    println(Codec.decode[Int]("123"))

    println(Codec.encode(Box(true)))
    println(Codec.decode[Box[Boolean]]("false"))
  }

}
