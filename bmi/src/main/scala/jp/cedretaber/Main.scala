package jp.cedretaber

import scala.util.Try

object Main {

  def bmi(h: Double, w: Double): Double = w / (h * h)

  def main2(args: Array[String]): Unit = {
    val h = args(0).toDouble / 100
    val w = args(1).toDouble

    println(bmi(h, w))
  }

  def main(args: Array[String]): Unit = {
    args.flatMap(s => Try { s.toDouble }.toOption) match {
      case Array(h, w) =>
        println(bmi(h / 100, w))
      case _ =>
        println("Invalid input.")
        sys.exit(1)
    }
  }
}
