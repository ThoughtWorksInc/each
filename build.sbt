libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.2"

libraryDependencies += "com.thoughtworks.each" %% "each" % "0.6.0"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.0-M15" % Test

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)

addCompilerPlugin("org.spire-math" % "kind-projector" % "0.7.1" cross CrossVersion.binary)

scalaVersion := "2.11.8"

scalacOptions += "-feature"