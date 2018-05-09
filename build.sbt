
lazy val commonSettings = Seq(
  version := "0.1",
  scalaVersion := "2.12.6"
)

lazy val bmi = (project in file("bmi")).settings(commonSettings)

lazy val highAndLow = (project in file("high-and-low")).settings(commonSettings)

lazy val blackjack = (project in file("blackjack")).settings(commonSettings)

lazy val reversi = (project in file("reversi")).settings(commonSettings)

lazy val aggregator = (project in file("aggregator")).settings(commonSettings)