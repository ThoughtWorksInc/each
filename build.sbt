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

lazy val root: Project = project in file(".") aggregate js

val compileSourceDirectory = settingKey[File]("Default directory containing sources for compile configuration")

compileSourceDirectory in js := (sourceDirectory in Compile).value

lazy val js: Project = project.addSbtFiles(file("../common.sbt")).settings(sourceDirectory in Compile := compileSourceDirectory.value)


libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % Test

libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.0.4" % Test

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.1.4"

libraryDependencies += "org.scalaz" %% "scalaz-effect" % "7.1.4"