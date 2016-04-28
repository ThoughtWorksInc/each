enablePlugins(ApiMappings)

libraryDependencies += "org.scalaz" %%% "scalaz-core" % "7.2.2"

libraryDependencies += "org.scalaz" %%% "scalaz-effect" % "7.2.2"

licenses := Seq("Apache License, Version 2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0.html"))

organization := "com.thoughtworks.each"

name := "each"

libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value

scalacOptions += "-deprecation"

scalacOptions += "-feature"

scalacOptions += "-unchecked"

description := "A macro library that converts native imperative syntax to scalaz's monadic expressions."

homepage := Some(url("https://github.com/ThoughtWorksInc/each"))

startYear := Some(2015)

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

releasePublishArtifactsAction := PgpKeys.publishSigned.value
