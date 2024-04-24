ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.2.2"

lazy val root = (project in file("."))
  .settings(
    name := "pfg"
  )

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-effect" % "3.5.3",
  "org.scalameta" %% "munit" % "0.7.29" % Test,
)


Compile / run / mainClass := Some("generator.Parser")