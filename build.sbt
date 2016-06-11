import ReleaseTransformations._

organization in ThisBuild := "com.thoughtworks.sde"

publishArtifact := false

crossScalaVersions in ThisBuild := Seq("2.10.6", "2.11.8", "2.12.0-M3")

description in ThisBuild := "A collection of Scala language extension for specific domains."

lazy val core = crossProject.crossType(CrossType.Pure)

lazy val coreJVM = core.jvm.addSbtFiles(file("../build.sbt.shared"))

lazy val coreJS = core.js.addSbtFiles(file("../build.sbt.shared"))

lazy val `comprehension-monad` = crossProject.crossType(CrossType.Pure)

lazy val `comprehension-monadJVM` = `comprehension-monad`.jvm.addSbtFiles(file("../build.sbt.shared"))

lazy val `comprehension-monadJS` = `comprehension-monad`.js.addSbtFiles(file("../build.sbt.shared"))

lazy val future = crossProject.crossType(CrossType.Pure).dependsOn(core)

lazy val futureJS = future.js.addSbtFiles(file("../build.sbt.shared"))

lazy val futureJVM = future.jvm.addSbtFiles(file("../build.sbt.shared"))

licenses in ThisBuild := Seq("Apache License, Version 2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0.html"))

homepage in ThisBuild := Some(url("https://github.com/ThoughtWorksInc/each"))

startYear in ThisBuild := Some(2015)

scmInfo in ThisBuild := Some(ScmInfo(
  url("https://github.com/ThoughtWorksInc/each"),
  "scm:git:git://github.com/ThoughtWorksInc/each.git",
  Some("scm:git:git@github.com:ThoughtWorksInc/each.git")))

developers in ThisBuild := List(
  Developer(
    "Atry",
    "杨博 (Yang Bo)",
    "pop.atry@gmail.com",
    url("https://github.com/Atry")
  ),
  Developer(
    "freewind",
    "Peng Li",
    "nowind_lee@qq.com",
    url("https://github.com/freewind")
  ),
  Developer(
    "zhanglongyang",
    "Longyang Zhang",
    "lyzhang@thoughtworks.com",
    url("https://github.com/zhanglongyang")
  ),
  Developer(
    "mengmeng0927",
    "Niu Yameng",
    "mengmeng0927@gmail.com",
    url("https://github.com/mengmeng0927")
  )
)

releaseProcess := {
  import xerial.sbt.Sonatype.SonatypeCommand.sonatypeReleaseAll
  releaseProcess.value.patch(
    releaseProcess.value.indexOf(pushChanges),
    Seq[ReleaseStep](
      releaseStepCommand(sonatypeReleaseAll, " com.thoughtworks.sde"),
      releaseStepCommand(sonatypeReleaseAll, " com.thoughtworks.each")
    ),
    0)
}

releaseProcess -= runClean

releaseProcess -= runTest
