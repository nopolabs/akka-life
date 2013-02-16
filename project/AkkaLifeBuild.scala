import sbt._
import sbt.Keys._

object AkkaLifeBuild extends Build {

  lazy val akkaLife = Project(
    id = "akka-life",
    base = file("."),
    settings = Project.defaultSettings ++ Seq(
      name := "Akka Life",
      organization := "com.nopolabs",
      version := "0.1-SNAPSHOT",
      scalaVersion := "2.9.2",
      resolvers += "Typesafe Releases" at "http://repo.typesafe.com/typesafe/releases",
      libraryDependencies += "com.typesafe.akka" % "akka-actor" % "2.0.1"
    )
  )
}
