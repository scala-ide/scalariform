import sbt._
import sbt.Keys._
//import ScalariformPlugin.{ format, formatPreferences }
//import scalariform.formatter.preferences._

object ScalariformBuild extends Build {

  lazy val buildSettings = Defaults.defaultSettings ++ Seq(
    organization := "scalariform",
    version      := "0.1.1",
    scalaVersion := "2.9.1",
    crossScalaVersions := Seq("2.8.0", "2.8.1", "2.8.2", "2.8.3-SNAPSHOT", "2.9.0", "2.9.0-1", "2.9.1", "2.10.0-SNAPSHOT"),
    resolvers += ScalaToolsSnapshots,
    retrieveManaged := true,
    scalacOptions += "-deprecation",
    logLevel in test := Level.Warn,
    pomExtra := pomExtraXml,
    publishMavenStyle := true,
    credentials += Credentials(Path.userHome / ".ivy2" / ".credentials")
  )  // ++ formatterSettings

  //lazy val formatterSettings = ScalariformPlugin.settings ++ Seq(
  //  formatPreferences in Compile := formattingPreferences,
   // formatPreferences in Test    := formattingPreferences
 // )

  // def formattingPreferences = PreferencesImporterExporter.loadPreferences("formatterPreferences.properties").asInstanceOf[FormattingPreferences]

  lazy val root = Project("root", file("."), settings = buildSettings) aggregate(
    scalariform, gui, perf, corpusScan)

  lazy val scalariform: Project = Project("scalariform", file("scalariform"), settings = buildSettings ++ 
    Seq(
      libraryDependencies <<= (scalaVersion, libraryDependencies) { (sv, deps) =>
         val scalatestVersion = sv match {
           case "2.8.0"           => "org.scalatest" %% "scalatest"       % "1.3.1.RC2" % "test"
           case "2.8.1"           => "org.scalatest" %% "scalatest"       % "1.5.1"     % "test"
           case "2.8.2"           => "org.scalatest" %% "scalatest"       % "1.5.1"     % "test"
           case "2.8.3-SNAPSHOT"  => "org.scalatest" %  "scalatest_2.8.2" % "1.5.1"     % "test"
           case "2.10.0-SNAPSHOT" => "org.scalatest" %  "scalatest_2.9.1" % "1.6.1"     % "test"
           case _                 => "org.scalatest" %% "scalatest"       % "1.6.1"     % "test"
         }
         deps :+ scalatestVersion
      },
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
