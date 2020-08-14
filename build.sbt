ThisBuild / scalaVersion := "2.13.3"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / organization := "com.example"
ThisBuild / organizationName := "example"

val zioVersion = "1.0.0"

lazy val root = (project in file("."))
  .settings(
    name := "scala-algorithms",
    libraryDependencies ++= Seq(
      "dev.zio"     %% "zio"           % zioVersion,
      "dev.zio"     %% "zio-test"      % zioVersion % "test",
      "dev.zio"     %% "zio-test-sbt"  % zioVersion % "test",
      "com.chuusai" % "shapeless_2.13" % "2.4.0-M1"
    )
  )
