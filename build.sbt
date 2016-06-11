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

releaseProcess := {
  import xerial.sbt.Sonatype.SonatypeCommand.sonatypeReleaseAll
  releaseProcess.value.patch(
    releaseProcess.value.indexOf(pushChanges),
    Seq[ReleaseStep](
      releaseStepCommand(sonatypeReleaseAll, " com.thoughtworks.each")
    ),
    0)
}
