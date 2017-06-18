publishArtifact := false

organization in ThisBuild := "com.thoughtworks.each"

crossScalaVersions in ThisBuild := Seq("2.11.11", "2.12.2")

lazy val EachPlugin = project.dependsOn(eachJVM % Provided, eachJVM % Test)

lazy val each = crossProject.crossType(CrossType.Pure).configureAll(_.addSbtFiles(file("../build.sbt.shared")))

lazy val eachJVM = each.jvm

lazy val eachJS = each.js

lazy val unidoc = project
  .enablePlugins(StandaloneUnidoc, TravisUnidocTitle)
  .settings(
    UnidocKeys.unidocProjectFilter in ScalaUnidoc in UnidocKeys.unidoc := {
      inProjects(eachJVM, EachPlugin)
    }
  )
