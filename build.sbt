publishArtifact := false

organization in ThisBuild := "com.thoughtworks.each"

crossScalaVersions in ThisBuild := Seq("2.11.11", "2.12.2")

lazy val each = project

lazy val unidoc = project
  .enablePlugins(StandaloneUnidoc, TravisUnidocTitle)
  .settings(
    UnidocKeys.unidocProjectFilter in ScalaUnidoc in UnidocKeys.unidoc := {
      inAggregates(LocalRootProject)
    }
  )
