enablePlugins(Travis)

releaseProcess := {
  import ReleaseTransformations._
  import xerial.sbt.Sonatype.SonatypeCommand.sonatypeRelease
  releaseProcess.value.patch(
    releaseProcess.value.indexOf(pushChanges),
    Seq[ReleaseStep](
      releaseStepCommand(sonatypeRelease, " com.thoughtworks.sde"),
      releaseStepCommand(sonatypeRelease, " com.thoughtworks.each")
    ),
    0
  )
}

lazy val secret = project settings(publishArtifact := false) configure { secret =>
  sys.env.get("GITHUB_PERSONAL_ACCESS_TOKEN") match {
    case Some(pat) =>
      import org.eclipse.jgit.transport.UsernamePasswordCredentialsProvider
      secret.addSbtFilesFromGit(
        "https://github.com/ThoughtWorksInc/tw-data-china-continuous-delivery-password.git",
        new UsernamePasswordCredentialsProvider(pat, ""),
        file("secret.sbt"))
    case None =>
      secret
  }
}
