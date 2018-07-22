name := "basil"

version := "0.1"

scalaVersion := "2.12.6"

libraryDependencies ++= Seq(
  "com.slamdata" %% "matryoshka-core" % "0.18.3",
  "org.spire-math" %% "jawn-parser" % "0.12.1",
  "org.json4s" %% "json4s-native" % "3.5.4",
  "org.scalatest" %% "scalatest" % "3.0.1" % "test",
  "org.scalacheck" %% "scalacheck" % "1.14.0" % "test"
)

scalacOptions += "-Ypartial-unification"
resolvers += Resolver.sonatypeRepo("releases")
addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.7")
