organization in ThisBuild := "com.thoughtworks.sde"

publish / skip := true

// Workaround for randomly Travis CI fail
parallelExecution in Global := false

fork in Global in compile := true

description in ThisBuild := "A collection of Scala language extension for specific domains."

lazy val core = crossProject.crossType(CrossType.Pure)

lazy val coreJVM = core.jvm

lazy val coreJS = core.js

lazy val each = crossProject.crossType(CrossType.Pure).dependsOn(core, `comprehension-monad`)

lazy val eachJVM = each.jvm

lazy val eachJS = each.js

lazy val `comprehension-monad` = crossProject.crossType(CrossType.Pure)

lazy val `comprehension-monadJVM` = `comprehension-monad`.jvm

lazy val `comprehension-monadJS` = `comprehension-monad`.js

lazy val future = crossProject.crossType(CrossType.Pure).dependsOn(core)

lazy val futureJS = future.js

lazy val futureJVM = future.jvm

lazy val source = crossProject.crossType(CrossType.Pure).dependsOn(core)

lazy val sourceJS = source.js

lazy val sourceJVM = source.jvm

lazy val gen = crossProject.crossType(CrossType.Pure).dependsOn(core)

lazy val genJS = gen.js

lazy val genJVM = gen.jvm

startYear in ThisBuild := Some(2015)
