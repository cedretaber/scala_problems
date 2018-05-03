package jp.cedretaber

object Main {
  def main(args: Array[String]): Unit = {
    val X = scala.util.Random.nextInt(9) + 1

    var N = 10
    var count = 0

    println("What number?")
    print("> ")
    while (X != N && count <= 5) {
      count += 1

      N = scala.io.StdIn.readInt

      if (N != X) {
        println(if (N > X) "High" else "Low")
        print("> ")
      }
    }

    println(if (X == N) "Correct!" else "Game Over!")
  }
}
