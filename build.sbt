lazy val common_project = Seq(
  organization := "prg20",
  version := "0.1-SNAPSHOT",

  fork in run          := true,
  connectInput in run  := true,
  cancelable in Global := true)

lazy val scala_project = common_project ++ Seq(
  scalaVersion := "2.13.3",   // コンパイルに使う scalac のバージョン
  scalacOptions := Seq("-feature", "-unchecked", "-deprecation"),

  // Scalaのプロジェクトのファイル構成を設定。
  // https://www.scala-sbt.org/1.x/docs/Multi-Project.html
  scalaSource in Compile := baseDirectory.value / "scala"
)

// サブプロジェクト群の定義。
lazy val root = (project in file(".")).settings(common_project)
lazy val othello = (project in file ("src/othello")).settings(scala_project)
