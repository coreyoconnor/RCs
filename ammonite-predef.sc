interp.load.ivy(
  "com.lihaoyi" %
  s"ammonite-shell_${scala.util.Properties.versionNumberString}" %
  ammonite.Constants.version
)
interp.load.ivy(
  "org.scalaz" %% "scalaz-core" % "7.2.7"
)
@
val shellSession = ammonite.shell.ShellSession()
import shellSession._
import ammonite.ops._
import ammonite.shell._

import scalaz._, Scalaz._

ammonite.shell.Configure(interp, repl, wd)

