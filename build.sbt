import ReleaseTransformations._

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
  releaseProcess.value.patch(releaseProcess.value.indexOf(pushChanges), Seq[ReleaseStep](releaseStepCommand("sonatypeRelease")), 0)
}

releaseProcess -= runClean

releaseProcess -= runTest
