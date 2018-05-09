package jp.cedretaber.reversi

sealed trait Stone {
  def inverse: Stone
  def show: String
}

case object Black extends Stone {
  val inverse: Stone = White
  val show: String = "黒"
}

case object White extends Stone {
  val inverse: Stone = Black
  val show: String = "白"
}