package jp.cedretaber

object Main {

  import scala.annotation.tailrec

  sealed trait Status
  final case object Hit extends Status
  final case object Stand extends Status

  type Cards = List[Int]

  val BustLimit = 21
  val DealerStandLimit = 16

  sealed trait Hands {
    val hands: Cards

    type Self

    def draw(rest: Cards): (Self, Cards)

    lazy val sum: Int = {
      val (sum, aces) =
        hands.foldLeft((0, 0)) {
          case ((s, a), 1) => (s + 1, a + 1)
          case ((s, a), 11 | 12 | 13) => (s + 10, a)
          case ((s, a), n) => (s + n, a)
        }

      @tailrec
      def ret(sum: Int, aces: Int): Int =
        aces match {
          case 0 => sum
          case _ if sum + 10 > BustLimit => sum
          case _ => ret(sum + 10, aces - 1)
        }

      ret(sum, aces)
    }

    lazy val isBust: Boolean = sum > BustLimit

    def showCards(hideFirst: Boolean = false): String = {
      hands.reverse.map {
        case 1 => "A"
        case 11 => "J"
        case 12 => "Q"
        case 13 => "K"
        case n => n.toString
      } match {
        case _ :: rest if hideFirst => "*" :: rest
        case hs => hs
      }
    }.mkString(" ")
  }

  final case class PlayerHands(hands: Cards) extends Hands {
    override type Self = PlayerHands

    override def draw(rest: Cards): (Self, Cards) = {
      val head :: tail = rest
      (PlayerHands(head :: hands), tail)
    }
  }

  final case class DealerHands(hands: Cards) extends Hands {
    override type Self = DealerHands

    lazy val isStand: Boolean = sum > DealerStandLimit

    override def draw(rest: Cards): (Self, Cards) =
      if (isStand)
        (this, rest)
      else {
        val head :: tail = rest
        (DealerHands(head :: hands), tail)
      }
  }

  def youWin = println("You win!")
  def youLose = println("You lose!")

  @tailrec
  def getAction: Status = {
    print("(H)it/(S)tand > ")
    scala.io.StdIn.readLine.toLowerCase match {
      case "hit" | "h" => Hit
      case "stand" | "s" => Stand
      case _ => getAction
    }
  }

  @tailrec
  def game(status: Status, playerHands: PlayerHands, dealerHands: DealerHands, rest: Cards, isFirst: Boolean = false): Unit = {

    lazy val isGameEnd = status == Stand && dealerHands.isStand

    println(s"dealer: ${dealerHands.showCards(isFirst)}")
    println(s"   you: ${playerHands.showCards()}")

    (playerHands.isBust, dealerHands.isBust) match {
      case (false, true) =>
        youWin
      case (true, _) =>
        youLose
      case _ if isGameEnd && playerHands.sum > dealerHands.sum =>
        youWin
      case _ if isGameEnd =>
        youLose
      case _ if status == Stand =>
        val (dHands, rest1) = dealerHands.draw(rest)
        game(status, playerHands, dHands, rest1)
      case _ if playerHands.sum == BustLimit =>
        game(Stand, playerHands, dealerHands, rest)
      case _ if dealerHands.isStand && playerHands.sum > dealerHands.sum =>
        youWin
      case _ =>
        val (action, pHands, rest1) =
          if (getAction == Hit) {
            val (pHands, rest1) = playerHands.draw(rest)
            (Hit, pHands, rest1)
          } else
            (Stand, playerHands, rest)

        val (dHands, rest2) = dealerHands.draw(rest1)
        game(action, pHands, dHands, rest2)
    }
  }

  def main(args: Array[String]): Unit = {
    val d1 :: d2 :: p1 :: p2 :: rest = scala.util.Random.shuffle(for (_ <- 1 to 4; i <- 1 to 13) yield i).toList
    game(Hit, PlayerHands(d1 :: d2 :: Nil), DealerHands(p1 :: p2 :: Nil), rest, true)
  }
}
