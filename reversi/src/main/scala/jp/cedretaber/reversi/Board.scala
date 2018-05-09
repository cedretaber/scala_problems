package jp.cedretaber.reversi

import scala.collection.BitSet

case class Board(stones: BitSet, flags: BitSet) {

  def put(x: Int, y: Int, stone: Stone): Board = {
    require(0 <= x && x < 8)
    require(0 <= y && y < 8)

    val pos = y*8+x
    val stones_ =
      stone match {
        case Black =>
          stones + pos
        case White =>
          stones - pos
      }

    Board(stones_, flags + pos)
  }

  def check(x: Int, y: Int): Option[Stone] = {
    require(0 <= x && x < 8)
    require(0 <= y && y < 8)

    val pos = y*8+x
    (flags(pos), stones(pos)) match {
      case (false, _) => None
      case (_, false) => Some(White)
      case (_, true) => Some(Black)
    }
  }

  lazy val count: (Int, Int) =
    (for {
      y <- 0 until 8
      x <- 0 until 8
    } yield (x, y)).foldLeft((0, 0)) {
      case ((b, w), (x, y)) =>
        check(x, y) match {
          case None => (b, w)
          case Some(Black) => (b+1, w)
          case Some(White) => (b, w+1)
        }
    }

  lazy val blackCount: Int = count._1
  lazy val whiteCount: Int = count._2

  def count(stone: Stone): Int =
    stone match {
      case Black => blackCount
      case White => whiteCount
    }

  lazy val toSeq: Seq[Seq[Option[Stone]]] =
    for {
      y <- 0 until 8
    } yield for {
      x <- 0 until 8
    } yield check(x, y)

  override def toString: String = {
    val lines =
      for ((line, i) <- toSeq.zipWithIndex) yield {
        val str = line.map {
          case Some(Black) => "*"
          case Some(White) => "o"
          case None => "_"
        }.mkString(" ")
        s"${i+1} $str"
      }
    "  a b c d e f g h" :: lines.toList mkString "\n"
  }
}

object Board {
  def init: Board =
    Board(BitSet.empty, BitSet.empty)
      .put(3, 3, White)
      .put(4, 3, Black)
      .put(3, 4, Black)
      .put(4, 4, White)
}
