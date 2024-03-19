ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.13"

lazy val root = (project in file("."))
  .settings(
    name := "Lab5"
  )

libraryDependencies ++= Seq(
  "org.json4s" %% "json4s-native" % "4.0.7",
  "org.json4s" %% "json4s-ext" % "4.0.7"
)
libraryDependencies += "com.lihaoyi" %% "ujson" % "3.1.4"
