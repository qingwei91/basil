name := "basil"

version := "0.1"

scalaVersion := "2.12.6"

libraryDependencies ++= Seq(
  "com.slamdata"         %% "matryoshka-core" % "0.18.3",
  "org.typelevel"        %% "cats-core"       % "1.3.1",
  "org.typelevel"        %% "cats-effect"     % "1.0.0",
  "com.github.mpilquist" %% "simulacrum"      % "0.13.0",
  "org.spire-math"       %% "jawn-parser"     % "0.12.1",
  "co.fs2"               %% "fs2-core"        % "1.0.0",
  "org.json4s"           %% "json4s-native"   % "3.5.4",
  "org.scalatest"        %% "scalatest"       % "3.0.1" % "test",
  "org.scalacheck"       %% "scalacheck"      % "1.14.0" % "test"
)

javaHome := Some(file("/Users/limqingwei/graalvm-ce-1.0.0-rc7/Contents/Home"))

scalacOptions += "-Ypartial-unification"
resolvers += Resolver.sonatypeRepo("releases")
addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.7")

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
