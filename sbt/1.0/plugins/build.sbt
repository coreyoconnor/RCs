// addSbtPlugin("org.ensime" % "sbt-ensime" % "2.5.1")
// addSbtPlugin("net.virtual-void" % "sbt-dependency-graph" % "0.9.0")

lazy val commandREPLPlugin = RootProject(file("./sbt-command-repl"))
// lazy val sbtixPlugin = RootProject(file("./sbtix/plugin"))

lazy val root = (project in file(".")).dependsOn()
