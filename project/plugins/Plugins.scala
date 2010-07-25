import sbt._

class Plugins(info: ProjectInfo) extends PluginDefinition(info) {

  val formatter = "com.github.olim7t" % "sbt-scalariform" % "1.0.0"

}
