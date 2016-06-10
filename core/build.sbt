enablePlugins(ApiMappings)

organization := "com.thoughtworks.sde"

name := "core"

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)

libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value % Provided

libraryDependencies += "org.typelevel" %% "macro-compat" % "1.1.1"

libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value

libraryDependencies ++= {
  if (scalaBinaryVersion.value == "2.10") {
    Seq("org.scalamacros" %% "quasiquotes" % "2.1.0")
  } else {
    Seq()
  }
}

scalacOptions += "-deprecation"

scalacOptions += "-feature"

scalacOptions += "-unchecked"

description := "A collection of Scala language extension for specific domains."

libraryDependencies += "org.scalaz" %%% "scalaz-core" % "7.2.2"

libraryDependencies += "org.scalaz" %%% "scalaz-effect" % "7.2.2"

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)

releasePublishArtifactsAction := PgpKeys.publishSigned.value

libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % Test

libraryDependencies += "com.thoughtworks.enableIf" %% "enableif" % "1.0.1" % Test
