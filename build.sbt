name := "luxoft"

version := "0.1"

scalaVersion := "2.13.1"

Compile/mainClass := Some("SensorProcessor")

libraryDependencies += "org.typelevel" %% "cats-core" % "2.0.0"
libraryDependencies += "org.specs2" %% "specs2-core" % "4.6.0" % "test"