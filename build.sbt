val scala3Version = "3.1.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "scala3-macros-playground",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
  ).settings(
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.11" % Test
  )
