Global / semanticdbEnabled := true

//addCompilerPlugin("com.github.cb372" % "scala-typed-holes" % "0.1.11" cross CrossVersion.full)
Global / onChangedBuildSource := ReloadOnSourceChanges

ThisBuild  / watchBeforeCommand := Watch.clearScreen

Global / allDocsSections ++= Seq(
    "akka-.*" -> (20, "Akka"),
    "cats-.*" -> (20, "Cats"),
    "spark-.*" -> (20, "Spark"),
    "log4j-.*" -> (30, "Logging (Log4j 2)"),
    "slf4j-.*" -> (30, "Logging (Log4j 2)"),
    "scala-.*" -> (10, "Scala Standard Libraries"),
    ".*" -> (999, "Other Included Libraries")
)

