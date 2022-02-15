import sbt._

object Dependencies {

  lazy val test = List(scalaTest).map(_ % Test)
  lazy val main = List(cats, catsEffect, http4sDsl, http4sCore, http4sClient, http4sBlazeServer, http4sBlazeClient)
  lazy val all = main ++ test

  lazy val scalaTest = "org.scalatest" %% "scalatest" % "3.2.9"
  lazy val cats = "org.typelevel" %% "cats-core" % "2.7.0"
  lazy val catsEffect = "org.typelevel" %% "cats-effect" % "3.3.5"
  lazy val http4sDsl = "org.http4s" %% "http4s-dsl" % "1.0.0-M31"
  lazy val http4sCore = "org.http4s" %% "http4s-core" % "1.0.0-M31"
  lazy val http4sClient = "org.http4s" %% "http4s-client" % "1.0.0-M31"
  lazy val http4sBlazeServer = "org.http4s" %% "http4s-blaze-server" % "1.0.0-M31"
  lazy val http4sBlazeClient = "org.http4s" %% "http4s-blaze-client" % "1.0.0-M31"

}
