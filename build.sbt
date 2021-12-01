name := "tapiridae"

version := "0.1"

ThisBuild / scalaVersion := "2.13.7"
ThisBuild / libraryDependencies += "com.softwaremill.sttp.tapir" %% "tapir-core" % "0.18.3"
ThisBuild / libraryDependencies += "com.softwaremill.sttp.tapir" %% "tapir-json-circe" % "0.18.3"

lazy val macros = (project in file("macros"))
  .settings(
    name := "tapiridae-macros",

    libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value
  )

lazy val root = (project in file("."))
  .dependsOn(macros)