SettingKey[Boolean]("autoStartServer", "").withRank(KeyRanks.Invisible) := false
ThisBuild / semanticdbEnabled := true
ThisBuild / semanticdbVersion := scalafixSemanticdb.revision
addCompilerPlugin("org.scalameta" % "semanticdb-scalac" % "4.3.24" cross CrossVersion.full)
ThisBuild / scalacOptions += {
  CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, n)) if n >= 13 => "-Wunused:imports"
    case _ => "-Ywarn-unused"
  }
}
