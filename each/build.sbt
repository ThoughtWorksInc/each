libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value % Provided

libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value % Provided

enablePlugins(Example)

organization in generateExample := "com.thoughtworks"
