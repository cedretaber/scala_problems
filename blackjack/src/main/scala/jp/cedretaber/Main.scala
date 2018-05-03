package jp.cedretaber

object Main {

  import scala.annotation.tailrec

  sealed trait Status
  final case object Hit extends Status
  final case object Stand extends Status

  type Cards = List[Int]

  val BustLimit = 21
  val DealerStandLimit = 16

  def sumPoints(hands: Cards): Int = {
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

  def isBust(hands: Cards): Boolean = sumPoints(hands) > BustLimit

  def playersDraw(hands: Cards, rest: Cards): (Cards, Cards) = {
    val head :: tail = rest
    (head :: hands, tail)
  }

  def isDealerStand(hands: Cards): Boolean = sumPoints(hands) > DealerStandLimit

  def dealersDraw(hands: Cards, rest: Cards): (Cards, Cards) =
    if (isDealerStand(hands))
      (hands, rest)
    else
      playersDraw(hands, rest)

  def youWin = println("You win!")
  def youLose = println("You lose!")

  def showCards(hands: Cards, hideFirst: Boolean = false): String = {
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
  def game(status: Status, playerHands: Cards, dealerHands: Cards, rest: Cards, isFirst: Boolean = false): Unit = {

    lazy val isGameEnd = status == Stand && isDealerStand(dealerHands)

    println(s"dealer: ${showCards(dealerHands, isFirst)}")
    println(s"   you: ${showCards(playerHands)}")

    (isBust(playerHands), isBust(dealerHands)) match {
      case (false, true) =>
        youWin
      case (true, _) =>
        youLose
      case _ if isGameEnd && sumPoints(playerHands) > sumPoints(dealerHands) =>
        youWin
      case _ if isGameEnd =>
        youLose
      case _ if status == Stand =>
        val (dHands, rest1) = dealersDraw(dealerHands, rest)
        game(status, playerHands, dHands, rest1)
      case _ if sumPoints(playerHands) == BustLimit =>
        game(Stand, playerHands, dealerHands, rest)
      case _ if isDealerStand(dealerHands) && sumPoints(playerHands) > sumPoints(dealerHands) =>
        youWin
      case _ =>
        getAction match {
          case Hit =>
            val (pHands, rest1) = playersDraw(playerHands, rest)
            val (dHands, rest2) = dealersDraw(dealerHands, rest1)
            game(Hit, pHands, dHands, rest2)
          case Stand =>
            val (dHands, rest1) = dealersDraw(dealerHands, rest)
            game(Stand, playerHands, dHands, rest1)
        }
    }
  }

  def main(args: Array[String]): Unit = {
    val d1 :: d2 :: p1 :: p2 :: rest = scala.util.Random.shuffle(for (_ <- 1 to 4; i <- 1 to 13) yield i).toList
    game(Hit, d1 :: d2 :: Nil, p1 :: p2 :: Nil, rest, true)
  }
}
