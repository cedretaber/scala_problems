package jp.cedretaber

object UseTrait {

  sealed trait Aggregator {
    val name: String

    protected def accumulate(elem: Double): Unit

    protected def result: Double

    def aggregate(seq: Seq[Double]): Double = {
      for (e <- seq) accumulate(e)
      result
    }
  }

  class Max extends Aggregator {
    val name = "max"

    private[this] var number = Double.MinValue

    override def accumulate(elem: Double): Unit = {
      if (elem > number) number = elem
    }

    override def result: Double = number
  }

  class Mean extends Aggregator {
    val name = "mean"

    private[this] var count: Double = 0
    private[this] var sum: Double = 0

    override def accumulate(elem: Double): Unit = {
      count += 1
      sum += elem
    }

    override def result: Double = sum / count
  }

  class Mode extends Aggregator {
    val name = "mode"

    private[this] var max: Double = 0
    private[this] var count: Int = 0
    private[this] var numbers: Map[Double, Int] = Map.empty

    override def accumulate(elem: Double): Unit = {
      val c =
        numbers.get(elem) match {
          case None =>
            numbers = numbers.updated(elem, 1)
            1
          case Some(ec) =>
            numbers = numbers.updated(elem, ec+1)
            ec + 1
        }
      if (c > count) {
        max = elem
        count = c
      }
    }

    override def result: Double = max
  }
}

object UseAbstractClass {

  sealed abstract class Aggregator(val name: String) {

    protected def accumulate(elem: Double): Unit

    protected def result: Double

    def aggregate(seq: Seq[Double]): Double = {
      for (e <- seq) accumulate(e)
      result
    }
  }

  class Max extends Aggregator("max") {

    private[this] var number = Double.MinValue

    override def accumulate(elem: Double): Unit = {
      if (elem > number) number = elem
    }

    override def result: Double = number
  }

  class Mean extends Aggregator("mean") {

    private[this] var count: Double = 0
    private[this] var sum: Double = 0

    override def accumulate(elem: Double): Unit = {
      count += 1
      sum += elem
    }

    override def result: Double = sum / count
  }

  class Mode extends Aggregator("mode") {

    private[this] var max: Double = 0
    private[this] var count: Int = 0
    private[this] var numbers: Map[Double, Int] = Map.empty

    override def accumulate(elem: Double): Unit = {
      val c =
        numbers.get(elem) match {
          case None =>
            numbers = numbers.updated(elem, 1)
            1
          case Some(ec) =>
            numbers = numbers.updated(elem, ec+1)
            ec + 1
        }
      if (c > count) {
        max = elem
        count = c
      }
    }

    override def result: Double = max
  }
}

object Main {

  import UseTrait._

  import scala.io.StdIn.readLine

  def main(args: Array[String]): Unit = {
    val aggregators =
      readLine
        .split("""\s+""")
        .map(_.toLowerCase)
        .map {
          case "max" => new Max
          case "mean" => new Mean
          case "mode" => new Mode
        }

    val numbers =
      readLine
        .split("""\s+""")
        .map(_.toDouble)

    for (aggregator <- aggregators) {
      println(s"${aggregator.name}: ${aggregator.aggregate(numbers)}")
    }
  }
}
