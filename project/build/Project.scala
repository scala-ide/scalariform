import sbt._

class Project(info: ProjectInfo) extends ParentProject(info) {

  lazy val main = project("scalariform", "scalariform", new CoreProject(_))
  lazy val gui = project("gui", "gui", new GuiProject(_), main)
  lazy val corpusScan = project("corpusscan", "corpusscan", new CorpusScanProject(_), main)

  val scalaToolsRepo = "Scala-Tools Maven Repository" at "http://scala-tools.org/repo-snapshots"
  val scalaToolsRepoReleases = "Scala-Tools Maven Repository" at "http://scala-tools.org/repo-releases"

  class CoreProject(info: ProjectInfo) extends DefaultProject(info) {

    override def artifactID = "scalariform"

    val scalatest = "org.scalatest" % "scalatest" % "1.2-for-scala-2.8.0.RC6-SNAPSHOT"

    override def mainClass = Some("scalariform.commandline.Main")

  }

  class CorpusScanProject(info: ProjectInfo) extends DefaultProject(info) {

    val commonsIo = "commons-io" % "commons-io" % "1.4"

    override def mainClass = Some("scalariform.corpusscan.Runner")

  }

  class GuiProject(info: ProjectInfo) extends DefaultProject(info) {

    val miglayout = "com.miglayout" % "miglayout" % "3.7.1"

    override def mainClass = Some("scalariform.gui.Main")

  }
}

