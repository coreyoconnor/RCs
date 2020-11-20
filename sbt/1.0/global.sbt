SettingKey[Boolean]("autoStartServer", "").withRank(KeyRanks.Invisible) := false
ThisBuild / semanticdbEnabled := true
ThisBuild / semanticdbVersion := scalafixSemanticdb.revision
addCompilerPlugin("org.scalameta" % "semanticdb-scalac" % "4.3.24" cross CrossVersion.full)
