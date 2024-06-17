ThisBuild / version := "1.0"

ThisBuild / scalaVersion := "3.3.1"

lazy val root = (project in file("."))
  .settings(
    name := "pfg"
  )
val blazeVersion = "0.23.16"
val http4sVersion = "0.23.27"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-effect" % "3.5.3",
  "de.sciss" % "scalamidi_2.11" % "0.2.1",
  "org.scalameta" %% "munit" % "0.7.29" % Test,
  "org.http4s" %% "http4s-ember-client" % http4sVersion,
  "org.http4s" %% "http4s-ember-server" % http4sVersion,
  "org.http4s" %% "http4s-dsl" % http4sVersion,
  "org.http4s" %% "http4s-circe" % http4sVersion,
  "io.circe" %% "circe-generic" % "0.14.7"
)
enablePlugins(JavaAppPackaging)

assembly / mainClass := Some("generator.GeneratorService")

assembly / assemblyJarName := "bach-machine.jar"

Compile / run / mainClass := Some("generator.GeneratorService")