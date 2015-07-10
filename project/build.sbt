lazy val root = project in file(".") dependsOn `sbt-api-mappings`

lazy val `sbt-api-mappings` = RootProject(uri("https://github.com/ThoughtWorksInc/sbt-api-mappings.git"))