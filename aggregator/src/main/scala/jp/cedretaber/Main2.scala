package jp.cedretaber

object Main2 {
  def fold(f: (Double, Double) => Double, init: Double, list: List[Double]): Double =
    list match {
      case Nil => init
      case head :: tail => fold(f, f(init, head), tail)
    }

  def max(list: List[Double]): Double = fold(Math.max, Double.MinValue, list)

  def sum(list: List[Double]): Double = fold(_ + _, 0, list)

  def length(list: List[Double]): Double = fold((a, _) => a + 1, 0, list)

  def mean(list: List[Double]): Double = sum(list) / length(list)

  def mode(list: List[Double], memo: Map[Double, Int] = Map.empty, m: Double = 0): Double =
    list match {
      case Nil => m
      case head :: tail =>
        val n = memo.getOrElse(head, 0) + 1
        mode(tail, memo + (head -> n), Math.max(n, m))
    }

  import scala.io.StdIn.readLine

  def main(args: Array[String]): Unit = {
    val aggregators =
      readLine
        .split("""\s+""")
        .map(_.toLowerCase)
        .map {
          case "max" => ("max", max _)
          case "mean" => ("mean", mean _)
        }

    val numbers =
      readLine
        .split("""\s+""")
        .map(_.toDouble)
        .toList

    for ((name, aggregator) <- aggregators) {
      println(s"$name: ${aggregator(numbers)}")
    }
  }
}
