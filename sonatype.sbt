import xerial.sbt.Sonatype._

publishTo := sonatypePublishTo.value
sonatypeProfileName := "io.github.qingwei91"
pomExtra := (
  <developers>
    <developer>
      <id>qingwei91</id>
      <name>QingWei</name>
      <url>https://github.com/qingwei91/</url>
    </developer>
  </developers>
)
publishMavenStyle := true
licenses := Seq("APL2" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt"))
useGpg := true
sonatypeProjectHosting := Some(GitHubHosting("qingwei", "basil", "l.q.wei91@gmail.com"))

credentials += Credentials(Path.userHome / ".sbt" / "sonatype_credential")
