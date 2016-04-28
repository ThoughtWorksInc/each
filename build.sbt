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

lazy val js: Project = project.enablePlugins(ScalaJSPlugin).addSbtFiles(file("../common.sbt")).settings(sourceDirectory in Compile := compileSourceDirectory.value)

libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % Test

libraryDependencies ++= {
  if (scalaBinaryVersion.value == "2.11") {
    Seq("org.scala-lang.modules" %% "scala-xml" % "1.0.4" % Test)
  } else {
    Seq()
  }
}
