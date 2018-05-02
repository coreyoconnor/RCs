libraryDependencies += "com.lihaoyi" % "ammonite" % "1.0.5" % "test" cross CrossVersion.full

initialCommands in (Test, console) := """ammonite.Main().run()"""

import org.ensime.EnsimeKeys._
ensimeJavaFlags in ThisBuild := Seq("-Xmx8g", "-XX:MaxMetaspaceSize=2g", "-XX:MaxDirectMemorySize=29965m")

ensimeServerJars in ThisBuild := Seq(BuildPaths.defaultGlobalBase / "ensime_2.12-2.0-SNAPSHOT-assembly.jar")
