import sbt._
import Keys._

object BlogScalaDslBuild extends Build {
  import Dependencies._

  lazy val project = Project(id = "BlogScalaDsl", base = file("."))
    .settings(organization := "com.github.jparkie")
    .settings(name := "BlogScalaDsl")
    .settings(scalaVersion := "2.11.7")
    .settings(libraryDependencies ++= projectDependencies)
}