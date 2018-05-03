package jp.cedretaber

object Main2 {
  def main(args: Array[String]): Unit = {

    val (max, count) =
      args.flatMap(s => scala.util.Try { s.toInt }.toOption) match {
        case Array(m, c) => (m, c)
        case Array(m) => (m, 5)
        case _ => (9, 5)
      }

    println(s"range: 1..$max, count: $count")

    val X = scala.util.Random.nextInt(max) + 1

    @scala.annotation.tailrec
    def game(c: Int): Unit =
      if (c == count)
        println("Game Over!")
      else {
        print("> ")
        val N = scala.io.StdIn.readInt
        if (X == N)
          println("Correct!")
        else {
          println(if (N > X) "High" else "Low")
          game(c+1)
        }
      }

    println("What number?")
    game(0)
  }
}
