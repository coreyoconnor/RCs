libraryDependencies += "com.lihaoyi" % "ammonite" % "0.7.7" % "test" cross CrossVersion.full

initialCommands in (Test, console) := """ammonite.Main().run()"""

