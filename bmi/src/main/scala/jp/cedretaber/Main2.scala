package jp.cedretaber

object Main2 {
  def main(args: Array[String]): Unit = {
    try {
      import scala.io.StdIn.readDouble
      print("身長（cm)を入力してください: ")
      val h = readDouble / 100
      print("体重（kg）を入力してください: ")
      val w = readDouble
      println(s"BMIは ${w / (h * h)} です。")
    } catch {
      case _: Exception =>
        println("不正な入力です。")
        sys.exit(1)
    }
  }
}
