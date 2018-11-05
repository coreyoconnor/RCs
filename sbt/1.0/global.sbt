/*
libraryDependencies += {
  val version = scalaBinaryVersion.value match {
    case "2.10" => "1.0.3"
    case _ ⇒ "1.1.2"
  }
  "com.lihaoyi" % "ammonite" % version % "test" cross CrossVersion.full
}

sourceGenerators in Test += Def.task {
  val file = (sourceManaged in Test).value / "amm.scala"
  IO.write(file, """object amm extends App { ammonite.Main.main(args) }""")
  Seq(file)
}.taskValue

import org.ensime.EnsimeKeys._
ensimeJavaFlags in ThisBuild := Seq("-Xmx8g", "-XX:MaxMetaspaceSize=2g", "-XX:MaxDirectMemorySize=30g")

ensimeServerJars in ThisBuild := {
  Seq(BuildPaths.defaultGlobalBase / s"ensime_${scalaBinaryVersion.value}-2.0-SNAPSHOT-assembly.jar")
}
 */
