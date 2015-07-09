licenses := Seq("Apache License, Version 2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0.html"))

scalaVersion := "2.11.7"

organization := "com.thoughtworks"

name := "each"

version := "0.1.0-SNAPSHOT"

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.1.1"

libraryDependencies += "org.scalaz" %% "scalaz-effect" % "7.1.1"

libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value

libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % Test

libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.0.4" % Test

scalacOptions += "-deprecation"

scalacOptions += "-feature"
