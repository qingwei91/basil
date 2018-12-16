import sbt.Keys.scalaVersion
import sbt.addCompilerPlugin
import xerial.sbt.Sonatype.GitHubHosting

javaHome := sys.env.get("GRAAL_HOME").map(s => file(s))

enablePlugins(TutPlugin)

lazy val basil = project
  .in(file("."))
  .settings(publishSettings) // needed to allow `sonatypeRelease`
  .aggregate(core, fs2, derive)

lazy val core = project
  .in(file("core"))
  .settings(name := "basil-core")
  .settings(commons)
  .settings(publishSettings)
  .settings(
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "1.4.0",
      "org.typelevel" %% "cats-free" % "1.4.0"
    ) ++ testDeps
  )

lazy val fs2 = project
  .in(file("fs2"))
  .dependsOn(core % "test->test;compile->compile")
  .settings(name := "basil-fs2")
  .settings(commons)
  .settings(publishSettings)
  .settings(
    libraryDependencies ++= Seq(
      "co.fs2"        %% "fs2-core"    % "1.0.2",
      "org.typelevel" %% "cats-effect" % "1.1.0"
    ) ++ testDeps
  )

lazy val derive = project
  .in(file("derive"))
  .dependsOn(core)
  .settings(name := "basil-derive")
  .settings(commons)
  .settings(publishSettings)
  .settings(
    libraryDependencies ++= Seq(
      "com.propensive" %% "magnolia" % "0.10.0"
    ) ++ testDeps
  )

lazy val benchmark = project
  .in(file("benchmark"))
  .dependsOn(core, derive)
  .settings(commons)
  .enablePlugins(JmhPlugin)
  .settings(
    libraryDependencies ++= Seq(
      "com.lihaoyi"       %% "upickle"              % "0.7.1",
      "com.dslplatform"   %% "dsl-json-scala"       % "1.8.4",
      "com.jsoniter"      % "jsoniter"              % "0.9.23",
      "io.circe"          %% "circe-generic"        % "0.10.1",
      "io.circe"          %% "circe-generic-extras" % "0.10.1",
      "io.circe"          %% "circe-parser"         % "0.10.1",
      "com.typesafe.play" %% "play-json"            % "2.7.0-RC2"
    ) ++ testDeps
  )

lazy val commons = Def.settings(
  scalaVersion := "2.12.6",
  organization := "io.github.qingwei91",
  scalacOptions ++= Seq(
    "-deprecation", // Emit warning and location for usages of deprecated APIs.
    "-encoding",
    "utf-8", // Specify character encoding used by source files.
    "-explaintypes", // Explain type errors in more detail.
    "-feature", // Emit warning and location for usages of features that should be imported explicitly.
    "-language:existentials", // Existential types (besides wildcard types) can be written and inferred
    "-language:experimental.macros", // Allow macro definition (besides implementation and application)
    "-language:higherKinds", // Allow higher-kinded types
    "-language:implicitConversions", // Allow definition of implicit functions called views
    "-unchecked", // Enable additional warnings where generated code depends on assumptions.
    "-Xcheckinit", // Wrap field accessors to throw an exception on uninitialized access.
    "-Xfatal-warnings", // Fail the compilation if there are any warnings.
    "-Xfuture", // Turn on future language features.
    "-Xlint:adapted-args", // Warn if an argument list is modified to match the receiver.
    "-Xlint:by-name-right-associative", // By-name parameter of right associative operator.
    "-Xlint:constant", // Evaluation of a constant arithmetic expression results in an error.
    "-Xlint:delayedinit-select", // Selecting member of DelayedInit.
    "-Xlint:doc-detached", // A Scaladoc comment appears to be detached from its element.
    "-Xlint:inaccessible", // Warn about inaccessible types in method signatures.
    "-Xlint:infer-any", // Warn when a type argument is inferred to be `Any`.
    "-Xlint:missing-interpolator", // A string literal appears to be missing an interpolator id.
    "-Xlint:nullary-override", // Warn when non-nullary `def f()' overrides nullary `def f'.
    "-Xlint:nullary-unit", // Warn when nullary methods return Unit.
    "-Xlint:option-implicit", // Option.apply used implicit view.
    "-Xlint:package-object-classes", // Class or object defined in package object.
    "-Xlint:poly-implicit-overload", // Parameterized overloaded implicit methods are not visible as view bounds.
    "-Xlint:private-shadow", // A private field (or class parameter) shadows a superclass field.
    "-Xlint:stars-align", // Pattern sequence wildcard must align with sequence component.
    "-Xlint:type-parameter-shadow", // A local type parameter shadows a type already in scope.
    "-Xlint:unsound-match", // Pattern match may not be typesafe.
    "-Yno-adapted-args", // Do not adapt an argument list (either by inserting () or creating a tuple) to match the receiver.
    "-Ypartial-unification", // Enable partial unification in type constructor inference
    "-Ywarn-dead-code", // Warn when dead code is identified.
    "-Ywarn-extra-implicit", // Warn when more than one implicit parameter section is defined.
    "-Ywarn-inaccessible", // Warn about inaccessible types in method signatures.
    "-Ywarn-infer-any", // Warn when a type argument is inferred to be `Any`.
    "-Ywarn-nullary-override", // Warn when non-nullary `def f()' overrides nullary `def f'.
    "-Ywarn-nullary-unit", // Warn when nullary methods return Unit.
    "-Ywarn-numeric-widen", // Warn when numerics are widened.
    "-Ywarn-unused:implicits", // Warn if an implicit parameter is unused.
    "-Ywarn-unused:imports", // Warn if an import selector is not referenced.
    "-Ywarn-unused:locals", // Warn if a local definition is unused.
    "-Ywarn-unused:params", // Warn if a value parameter is unused.
    "-Ywarn-unused:patvars", // Warn if a variable bound in a pattern is unused.
    "-Ywarn-unused:privates", // Warn if a private member is unused.
    "-Ywarn-value-discard" // Warn when non-Unit expression results are unused.
  ),
  resolvers += Resolver.sonatypeRepo("releases"),
  addCompilerPlugin("org.spire-math"  %% "kind-projector" % "0.9.7"),
  addCompilerPlugin("org.scalamacros" % "paradise"        % "2.1.0" cross CrossVersion.full)
)

lazy val publishSettings = Def.settings(
  sonatypeProfileName := "io.github.qingwei91",
  publishMavenStyle := true,
  licenses := Seq("APL2" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt")),
  useGpg := true,
  sonatypeProjectHosting := Some(GitHubHosting("qingwei", "basil", "l.q.wei91@gmail.com")),
  credentials += Credentials(Path.userHome / ".sbt" / "sonatype_credential")
)

publishTo in ThisBuild := sonatypePublishTo.value
dynverSonatypeSnapshots in ThisBuild := true

lazy val testDeps = Seq(
  "org.json4s"     %% "json4s-native" % "3.5.4"  % "test",
  "com.lihaoyi"    %% "pprint"        % "0.5.3"  % "test",
  "org.scalatest"  %% "scalatest"     % "3.0.1"  % "test",
  "org.scalacheck" %% "scalacheck"    % "1.14.0" % "test"
)
