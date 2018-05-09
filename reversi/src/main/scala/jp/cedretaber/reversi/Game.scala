package jp.cedretaber.reversi

sealed trait Game

final case class Playing(board: Board, turn: Stone) extends Game {
  def putStone(): Game = {
    tryAll.toList match {
      case Nil =>
        val newPlaying = this.copy(turn = turn.inverse)
        if (newPlaying.canPut)
          newPlaying
        else
          Finished(board)
      case list =>
        val (max, head) :: tail =
          list
            .map { b => (b.count(turn), b) }
            .sortBy { case (c, _) => -c }
        val newBoard =
          scala.util.Random.shuffle(head :: tail.takeWhile { case (c, _) => c == max }.map(_._2)).head

        Playing(newBoard, turn.inverse)
    }
  }

  def putStone(x: Int, y: Int): Option[Playing] = {
    tryPut(x, y).map(b => Playing(b, turn.inverse))
  }

  lazy val canPut: Boolean = tryAll.nonEmpty

  def tryPut(x: Int, y: Int): Option[Board] =
    if (outOfBoard(x, y))
      None
    else
      board.check(x, y) match {
        case Some(_) =>
          None
        case None =>
          val newBoard = flipStones(x, y)
          if (newBoard.count(turn) > board.count(turn))
            Some(newBoard.put(x, y, turn))
          else
            None
      }

  lazy val tryAll: Seq[Board] =
    for {
      x <- 0 until 8
      y <- 0 until 8
      newBoard <- tryPut(x, y)
      if newBoard.count(turn) > board.count(turn)
    } yield newBoard

  val Directions = Seq(1, -1, 0)

  def flipStones(x: Int, y: Int): Board =
    (for {
      xd <- Directions
      yd <- Directions
      if xd != 0 || yd != 0
    } yield (xd, yd)).foldLeft(board) {
      case (board_, (xd, yd)) =>
        def searchAndFlip(xp: Int, yp: Int): (Boolean, Board) =
          if (outOfBoard(xp, yp))
            (false, board_)
          else {
            board_.check(xp, yp) match {
              case None =>
                (false, board_)
              case Some(s) if s == turn =>
                (true, board_)
              case Some(_) =>
                searchAndFlip(xp + xd, yp + yd) match {
                  case (false, _) =>
                    (false, board_)
                  case (true, bd) =>
                    (true, bd.put(xp, yp, turn))
                }
            }
          }
        searchAndFlip(x + xd, y + yd)._2
    }

  private[this] def outOfBoard(x: Int, y: Int): Boolean =
    x < 0 || y < 0 || x >= 8 || y >= 8
}

final case class Finished(board: Board) extends Game