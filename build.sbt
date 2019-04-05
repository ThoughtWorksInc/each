organization in ThisBuild := "com.thoughtworks.each"

publish / skip := true

// Workaround for randomly Travis CI fail
parallelExecution in Global := false

fork in Global in compile := true

description in ThisBuild := "A collection of Scala language extension for specific domains."

lazy val each = crossProject.crossType(CrossType.Pure)

lazy val eachJVM = each.jvm

lazy val eachJS = each.js

startYear in ThisBuild := Some(2015)
