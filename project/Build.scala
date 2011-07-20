import sbt._
import sbt.Keys._
import ScalariformPlugin.{ format, formatPreferences }
import scalariform.formatter.preferences._

object MyBuild extends Build {

  lazy val buildSettings = Defaults.defaultSettings ++ Seq(
    organization := "scalariform",
    version      := "0.1.1-SNAPSHOT",
    scalaVersion := "2.9.0",
    crossScalaVersions := Seq("2.8.0", "2.8.1", "2.8.2-SNAPSHOT", "2.9.0", "2.9.0-1", "2.9.1-SNAPSHOT"),
    resolvers += ScalaToolsSnapshots,
    retrieveManaged := true,
    scalacOptions += "-deprecation",
    logLevel in test := Level.Warn,
    pomExtra := pomExtraXml,
    publishMavenStyle := true,
    credentials += Credentials(Path.userHome / ".ivy2" / ".credentials")
  ) ++ formatterSettings

  lazy val formatterSettings = ScalariformPlugin.settings ++ Seq(
    formatPreferences in Compile := formattingPreferences,
    formatPreferences in Test    := formattingPreferences
  )

  def formattingPreferences = PreferencesImporterExporter.loadPreferences("formatterPreferences.properties").asInstanceOf[FormattingPreferences]

  lazy val root = Project("root", file("."), settings = buildSettings) aggregate(
    scalariform, gui, perf, corpusScan)

  lazy val scalariform: Project = Project("scalariform", file("scalariform"), settings = buildSettings ++ 
    Seq(
      libraryDependencies += "org.scalatest" % "scalatest_2.9.0" % "1.6.1"  % "test",
      mainClass in (Compile, packageBin) := Some("scalariform.commandline.Main"),
      publishTo <<= version { (v: String) =>
        if (v endsWith "-SNAPSHOT")
          Some(ScalaToolsSnapshots)
        else
          Some(ScalaToolsReleases)
      }
    ),
    delegates = root :: Nil)

  lazy val perf: Project = Project("perf", file("perf"), settings = buildSettings ++
    Seq()) dependsOn(scalariform)

  lazy val corpusScan: Project = Project("corpusscan", file("corpusscan"), settings = buildSettings ++
    Seq(
      libraryDependencies += "commons-io" % "commons-io" % "1.4"
    )) dependsOn(scalariform)

  lazy val gui: Project = Project("gui", file("gui"), settings = buildSettings ++
    Seq(
      libraryDependencies += "com.miglayout" % "miglayout" % "3.7.4",
      mainClass in (Compile, run) := Some("scalariform.gui.Main")
    )) dependsOn(scalariform)


   def pomExtraXml =
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
