libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value % Provided

libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value % Provided

enablePlugins(Example)

organization in generateExample := "com.thoughtworks"

scalacOptions in Test += "-Xprint:typer"

libraryDependencies += "org.typelevel" %% "cats" % "0.9.0" % Test
