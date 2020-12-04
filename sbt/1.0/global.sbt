SettingKey[Boolean]("autoStartServer", "").withRank(KeyRanks.Invisible) := false
ThisBuild / semanticdbEnabled := true
ThisBuild / semanticdbVersion := scalafixSemanticdb.revision
addCompilerPlugin("org.scalameta" % "semanticdb-scalac" % "4.4.0" cross CrossVersion.full)
addCompilerPlugin("com.github.cb372" % "scala-typed-holes" % "0.1.6" cross CrossVersion.full)

