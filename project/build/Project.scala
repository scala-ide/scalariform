import sbt._
import com.github.olim7t.sbtscalariform._
import reaktor.scct.ScctProject

class Project(info: ProjectInfo) extends ParentProject(info) {

  trait FormatterOptions /* extends ScalariformPlugin */ {

    /* override */ def scalariformOptions = Seq(VerboseScalariform, RewriteArrowSymbols(true), AlignParameters(true) /* AlignSingleLineCaseStatements(true) */ )

  }

  lazy val main = project("scalariform", "scalariform", new CoreProject(_))
  lazy val gui = project("gui", "gui", new GuiProject(_), main)
  lazy val corpusScan = project("corpusscan", "corpusscan", new CorpusScanProject(_), main)
  lazy val perf = project("perf", "perf", new PerformanceProject(_), main)

  val scalaToolsRepo = "Scala-Tools Maven Repository" at "http://scala-tools.org/repo-snapshots"
  val scalaToolsRepoReleases = "Scala-Tools Maven Repository" at "http://scala-tools.org/repo-releases"

  class CoreProject(info: ProjectInfo) extends DefaultProject(info) with FormatterOptions with posterous.Publish with ScctProject {

    val scalatest = "org.scalatest" % "scalatest" % "1.2" % "test->default"

    override def mainClass = Some("scalariform.commandline.Main")

    override def managedStyle = ManagedStyle.Maven

    //val publishTo = "Scala Toos Nexus" at "http://nexus.scala-tools.org/content/repositories/releases/"
    val publishTo = "Scala Toos Nexus" at "http://nexus.scala-tools.org/content/repositories/snapshots/"

    //val publishTo = Resolver.file("maven-local", Path.userHome / ".m2" / "repository" asFile) 

    Credentials(Path.userHome / ".ivy2" / ".credentials", log)

    override def pomExtra =
      <inceptionYear>2010</inceptionYear>
      <url>http://github.com/mdr/scalariform</url>
      <licenses>
        <license>
          <name>MIT License</name>
          <url>http://www.opensource.org/licenses/mit-license.php</url>
          <distribution>repo</distribution>
        </license>
      </licenses>

  }

  class CorpusScanProject(info: ProjectInfo) extends DefaultProject(info) with FormatterOptions {

    val commonsIo = "commons-io" % "commons-io" % "1.4"

    override def mainClass = Some("scalariform.corpusscan.Runner")

  }

  class PerformanceProject(info: ProjectInfo) extends DefaultProject(info) with FormatterOptions {

    override def mainClass = Some("scalariform.perf.LexerPerformanceTest")

  }

  class GuiProject(info: ProjectInfo) extends DefaultProject(info) with FormatterOptions {

    val miglayout = "com.miglayout" % "miglayout" % "3.7.1"

    override def mainClass = Some("scalariform.gui.Main")

  }

}

