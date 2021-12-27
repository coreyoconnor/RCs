val sonatypeCredsPath = Path.userHome / ".sbt" / "dhb-sonatype-credentials"

credentials ++= {
  if (sonatypeCredsPath.exists) Seq(Credentials(sonatypeCredsPath)) else Seq.empty
}

