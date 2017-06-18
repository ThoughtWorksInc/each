libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value % Provided

scalacOptions in Test += "-Xplugin:" + (packageBin in Compile).value

enablePlugins(Example)

organization in generateExample := "com.thoughtworks"
