name := "COMP442-Jacob-Gagné"

version := "0.1"

scalaVersion := "2.12.8"

mainClass in (Compile,run) := Some("Compiler")

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % "7.2.27",
  "org.scalatest" %% "scalatest" % "3.0.5" % Test
)