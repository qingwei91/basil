publishTo := sonatypePublishTo.value
sonatypeProfileName := "io.github.qingwei91"
publishMavenStyle := true
licenses := Seq("APL2" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt"))
useGpg := true

import xerial.sbt.Sonatype._
sonatypeProjectHosting := Some(GitHubHosting("qingwei", "basil", "l.q.wei91@gmail.com"))

credentials += Credentials(Path.userHome / ".sbt" / "sonatype_credential")
