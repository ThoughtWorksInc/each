import ReleaseTransformations._

releaseProcess := Seq[ReleaseStep](
  checkSnapshotDependencies,
  inquireVersions,
  runClean,
  runTest,
  setReleaseVersion,
  commitReleaseVersion,
  tagRelease,
  publishArtifacts,
  setNextVersion,
  commitNextVersion,
  releaseStepCommand("sonatypeRelease"),
  pushChanges
)

publishArtifact := false

scalaVersion in ThisBuild := "2.11.7"

lazy val sde = crossProject.crossType(CrossType.Pure)

lazy val sdeJS = sde.js.addSbtFiles(file("../build.sbt.shared"))

lazy val sdeJVM = sde.jvm.addSbtFiles(file("../build.sbt.shared"))

lazy val each = crossProject.crossType(CrossType.Pure) dependsOn sde

lazy val eachJS = each.js.addSbtFiles(file("../build.sbt.shared"))

lazy val eachJVM = each.jvm.addSbtFiles(file("../build.sbt.shared"))

licenses in ThisBuild := Seq("Apache License, Version 2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0.html"))

homepage in ThisBuild  := Some(url("https://github.com/ThoughtWorksInc/each"))

startYear in ThisBuild  := Some(2015)

scmInfo in ThisBuild := Some(ScmInfo(
  url("https://github.com/ThoughtWorksInc/each"),
  "scm:git:git://github.com/ThoughtWorksInc/each.git",
  Some("scm:git:git@github.com:ThoughtWorksInc/each.git")))

pomExtra in ThisBuild :=
  <developers>
    <developer>
      <id>Atry</id>
      <name>杨博 (Yang Bo)</name>
      <timezone>+8</timezone>
      <email>pop.atry@gmail.com</email>
    </developer>
    <developer>
      <id>freewind</id>
      <name>Peng Li</name>
      <timezone>+8</timezone>
      <email>nowind_lee@qq.com</email>
    </developer>
    <developer>
      <id>zhanglongyang</id>
      <name>Longyang Zhang</name>
      <timezone>+8</timezone>
      <email>lyzhang@thoughtworks.com</email>
    </developer>
    <developer>
      <id>mengmeng0927</id>
      <name>Niu Yameng</name>
      <timezone>+8</timezone>
      <email>mengmeng0927@gmail.com</email>
    </developer>
  </developers>
