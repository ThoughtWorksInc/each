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

scalaVersion in ThisBuild := "2.11.7"

lazy val root: Project = project in file(".") aggregate js

val compileSourceDirectory = settingKey[File]("Default directory containing sources for compile configuration")

compileSourceDirectory in js := (sourceDirectory in Compile).value

lazy val js: Project = project.addSbtFiles(file("../common.sbt")).settings(sourceDirectory in Compile := compileSourceDirectory.value)


libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % Test

libraryDependencies ++= {
  if (scalaBinaryVersion.value == "2.11") {
    Seq("org.scala-lang.modules" %% "scala-xml" % "1.0.4" % Test)
  } else {
    Seq()
  }
}

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.0"

libraryDependencies += "org.scalaz" %% "scalaz-effect" % "7.2.0"

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
