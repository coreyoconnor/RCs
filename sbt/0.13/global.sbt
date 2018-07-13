import org.ensime.EnsimeKeys._

// libraryDependencies += "com.lihaoyi" % "ammonite" % "1.0.5" % "test" cross CrossVersion.full

// initialCommands in (Test, console) := """ammonite.Main().run()"""

ensimeJavaFlags in ThisBuild := Seq("-Xmx8g", "-XX:MaxMetaspaceSize=2g", "-XX:MaxDirectMemorySize=29965m")

import org.ensime.EnsimeCoursierKeys._
ensimeServerVersion in ThisBuild := "2.0.1"
