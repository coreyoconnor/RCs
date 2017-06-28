libraryDependencies += "com.lihaoyi" % "ammonite" % "0.8.3" % "test" cross CrossVersion.full

initialCommands in (Test, console) := """ammonite.Main().run()"""

import org.ensime.EnsimeKeys._
ensimeJavaFlags in ThisBuild := Seq("-Xmx8g", "-XX:MaxMetaspaceSize=2g")

/*
import org.ensime.EnsimeCoursierKeys._
ensimeServerVersion in ThisBuild := "2.0.0-SNAPSHOT"
*/
