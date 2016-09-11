organization in ThisBuild := "com.thoughtworks.sde"

publishArtifact := false

// Workaround for randomly Travis CI fail
parallelExecution in Global := false

fork in Global := true

crossScalaVersions in ThisBuild := Seq("2.10.6", "2.11.8", "2.12.0-RC1")

description in ThisBuild := "A collection of Scala language extension for specific domains."

lazy val core = crossProject.crossType(CrossType.Pure)

lazy val coreJVM = core.jvm.addSbtFiles(file("../build.sbt.shared"))

lazy val coreJS = core.js.addSbtFiles(file("../build.sbt.shared"))

lazy val each = crossProject.crossType(CrossType.Pure).dependsOn(core, `comprehension-monad`)

lazy val eachJVM = each.jvm.addSbtFiles(file("../build.sbt.shared"))

lazy val eachJS = each.js.addSbtFiles(file("../build.sbt.shared"))

lazy val `comprehension-monad` = crossProject.crossType(CrossType.Pure)

lazy val `comprehension-monadJVM` = `comprehension-monad`.jvm.addSbtFiles(file("../build.sbt.shared"))

lazy val `comprehension-monadJS` = `comprehension-monad`.js.addSbtFiles(file("../build.sbt.shared"))

lazy val future = crossProject.crossType(CrossType.Pure).dependsOn(core)

lazy val futureJS = future.js.addSbtFiles(file("../build.sbt.shared"))

lazy val futureJVM = future.jvm.addSbtFiles(file("../build.sbt.shared"))

lazy val source = crossProject.crossType(CrossType.Pure).dependsOn(core)

lazy val sourceJS = source.js.addSbtFiles(file("../build.sbt.shared"))

lazy val sourceJVM = source.jvm.addSbtFiles(file("../build.sbt.shared"))

lazy val gen = crossProject.crossType(CrossType.Pure).dependsOn(core)

lazy val genJS = gen.js.addSbtFiles(file("../build.sbt.shared"))

lazy val genJVM = gen.jvm.addSbtFiles(file("../build.sbt.shared"))

startYear in ThisBuild := Some(2015)

scalaJSUseRhino in ThisBuild := false
