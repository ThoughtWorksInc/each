licenses := Seq("Apache License, Version 2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0.html"))

scalaVersion := "2.11.7"

organization := "com.thoughtworks"

name := "each"

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.1.1"

libraryDependencies += "org.scalaz" %% "scalaz-effect" % "7.1.1"

libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value

libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % Test

libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.0.4" % Test

scalacOptions += "-deprecation"

scalacOptions += "-feature"

releasePublishArtifactsAction := PgpKeys.publishSigned.value

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
  releaseStepCommand("sonatypeRelease"),
  setNextVersion,
  commitNextVersion,
  pushChanges
)

description := "A macro library that converts native imperative syntax to scalaz's monadic expressions."

homepage := Some(url("https://github.com/ThoughtWorksInc/each"))

startYear := Some(2015)

publishTo <<= (isSnapshot) { isSnapshot: Boolean =>
  if (isSnapshot)
    Some("snapshots" at "https://oss.sonatype.org/content/repositories/snapshots")
  else
    Some("releases" at "https://oss.sonatype.org/service/local/staging/deploy/maven2")
}

scmInfo := Some(ScmInfo(
  url("https://github.com/ThoughtWorksInc/each"),
  "scm:git:git://github.com/ThoughtWorksInc/each.git",
  Some("scm:git:git@github.com:ThoughtWorksInc/each.git")))

pomExtra :=
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
