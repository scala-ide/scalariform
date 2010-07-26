import sbt._

class Plugins(info: ProjectInfo) extends PluginDefinition(info) {

  //val scalaToolsRepo = "Scala-Tools Maven Repository" at "http://scala-tools.org/repo-snapshots"
  //val formatter = "com.github.olim7t" % "sbt-scalariform" % "1.0.1-SNAPSHOT"

  val formatter = "com.github.olim7t" % "sbt-scalariform" % "1.0.0"

}
