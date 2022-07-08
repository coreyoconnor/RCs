Global / semanticdbEnabled := true

//ThisBuild / semanticdbVersion := scalafixSemanticdb.revision
//addCompilerPlugin("org.scalameta" % "semanticdb-scalac" % "4.4.31" cross CrossVersion.full)
//addCompilerPlugin("com.github.cb372" % "scala-typed-holes" % "0.1.8" cross CrossVersion.full)
Global / onChangedBuildSource := ReloadOnSourceChanges

ThisBuild  / watchBeforeCommand := Watch.clearScreen
