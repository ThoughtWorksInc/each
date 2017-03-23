organization in ThisBuild := "com.thoughtworks.sde"

publishArtifact := false

// Workaround for randomly Travis CI fail
parallelExecution in Global := false

fork in Global in compile := true

crossScalaVersions in ThisBuild := Seq("2.10.6", "2.11.8", "2.12.1")

description in ThisBuild := "A collection of Scala language extension for specific domains."

lazy val core = crossProject.crossType(CrossType.Pure).configureAll(_.addSbtFiles(file("../build.sbt.shared")))

lazy val coreJVM = core.jvm

lazy val coreJS = core.js

lazy val CatsAdapter = crossProject
  .crossType(CrossType.Pure)
  .dependsOn(core % Test)
  .configureAll(_.addSbtFiles(file("../build.sbt.shared")))

lazy val CatsAdapterJVM = CatsAdapter.jvm

lazy val CatsAdapterJS = CatsAdapter.js

lazy val ScalazAdapter = crossProject
  .crossType(CrossType.Pure)
  .dependsOn(core % Test)
  .configureAll(_.addSbtFiles(file("../build.sbt.shared")))

lazy val ScalazAdapterJVM = ScalazAdapter.jvm

lazy val ScalazAdapterJS = ScalazAdapter.js

startYear in ThisBuild := Some(2015)
