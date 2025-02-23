import sbt.Keys.organization

ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "3.3.5"

val scala3Version = "3.6.3"
val zioVersion = "2.1.15"

lazy val root = (project in file("."))
  .settings(
    name := "zio-playground",
    organization := "com.rezo",
    libraryDependencies ++= Seq(
      "com.typesafe" % "config" % "1.4.3",
      "com.github.pureconfig" %% "pureconfig-core" % "0.17.8",
      "dev.zio" %% "zio" % zioVersion,
      "dev.zio" %% "zio-http" % "3.0.1",
      "dev.zio" %% "zio-nio" % "2.0.2",
      "dev.zio" %% "zio-streams" % "2.1.15",
      "dev.zio" %% "zio-kafka" % "2.11.0",
      "org.apache.kafka" % "kafka-clients" % "3.9.0",
      "dev.zio" %% "zio-test" % zioVersion,
      "dev.zio" %% "zio-test-sbt" % zioVersion
    )
  )
