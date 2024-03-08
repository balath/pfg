ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.2.2"

lazy val root = (project in file("."))
  .settings(
    name := "pfg"
  )

libraryDependencies += "org.typelevel" %% "cats-effect" % "3.5.3"
libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test