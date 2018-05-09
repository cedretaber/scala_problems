package jp.cedretaber

import jp.cedretaber.reversi._

object Main {

  object CommandLineAdapter {

    def play: Unit = {
      val (playing, colour) = startGame()
      val fin = run(playing, colour)
      showResult(fin, colour)
    }

    import scala.annotation.tailrec

    val c2i = "abcdefgh".split("").zipWithIndex.toMap

    def startGame(): (Playing, Stone) = {
      val stone =
        if (scala.util.Random.nextInt(2) == 0)
          Black
        else
          White
      println(s"あたなは ${stone.show} です。")
      (Playing(Board.init, Black), stone)
    }

    def run(game: Playing, playerColour: Stone): Finished = {

      val position = """(\w)\s*(\d)""".r

      @tailrec
      def readPosition: (Int, Int) = {
        print("> ")
        scala.io.StdIn.readLine match {
          case position(xs, yi) =>
            (c2i.get(xs), yi.toInt-1) match {
              case (Some(x), y) if 0 <= y && y < 8 =>
                (x, y)
              case _ =>
                println("不正な入力です。")
                readPosition
            }
          case _ =>
            println("不正な入力です。")
            readPosition
        }
      }

      @tailrec
      def go(game: Playing): Finished =
        if (game.turn == playerColour) {
          game.putStone() match {
            case fin: Finished =>
              fin
            case _: Playing =>
              println(game.board.toString)

              val (x, y) = readPosition
              game.putStone(x, y) match {
                case None =>
                  println("その位置には置けません。")
                  go(game)
                case Some(newGame) =>
                  go(newGame)
              }
          }
        } else
          game.putStone() match {
            case fin: Finished => fin
            case play: Playing => go(play)
          }
      go(game)
    }

    def showResult(fin: Finished, colour: Stone): Unit = {
      println(fin.board.toString)

      val (black, white) = fin.board.count

      println(s"黒: $black 対 白: $white")
      println {
        if (black == white) {
          "引き分けです。"
        } else if ((black > white && colour == Black) || (white > black && colour == White)) {
          "あなたの勝ちです。"
        } else {
          "あなたの負けです。"
        }
      }
    }
  }

  def main(args: Array[String]): Unit = {
    CommandLineAdapter.play
  }
}
