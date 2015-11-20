Nice.scalaProject

name          := "rephrase"
organization  := "ohnosequences"
description   := "rephrase project"

bucketSuffix  := "era7.com"

libraryDependencies ++= Seq(
  "ohnosequences" %% "cosas" % "0.8.0-SNAPSHOT",
  "org.scalatest" %% "scalatest" % "2.2.2" % Test
)
