import sbt._

class Plugins(info: ProjectInfo) extends PluginDefinition(info) {

  //val scalaToolsRepo = "Scala-Tools Maven Repository" at "http://scala-tools.org/repo-snapshots"
  //val formatter = "com.github.olim7t" % "sbt-scalariform" % "1.0.1-SNAPSHOT"

  val formatter = "com.github.olim7t" % "sbt-scalariform" % "1.0.0"

  val snuggletex_repo = "snuggletex_repo" at "http://www2.ph.ed.ac.uk/maven2"
  val t_repo = "t_repo" at "http://tristanhunt.com:8081/content/groups/public/"
  val posterous = "net.databinder" % "posterous-sbt" % "0.1.6"

  val scctRepo = "scct-repo" at "http://mtkopone.github.com/scct/maven-repo/"
  lazy val scctPlugin = "reaktor" % "sbt-scct-for-2.8" % "0.1-SNAPSHOT"

}
