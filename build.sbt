libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value

libraryDependencies += "com.thoughtworks.extractor" %% "extractor" % "2.1.0"

addCompilerPlugin("org.scalameta" % "paradise" % "3.0.0-M7" cross CrossVersion.patch)

libraryDependencies += "org.scalameta" %% "scalameta" % "1.6.0"
