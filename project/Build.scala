import sbt._
import sbt.Keys._
import com.github.retronym.SbtOneJar
import com.typesafe.sbteclipse.core.EclipsePlugin.EclipseKeys._
import com.typesafe.sbteclipse.core.EclipsePlugin._

import com.typesafe.sbtscalariform.ScalariformPlugin
import com.typesafe.sbtscalariform.ScalariformPlugin.ScalariformKeys
import scalariform.formatter.preferences._

object ScalariformBuild extends Build {

  lazy val commonSettings = Defaults.defaultSettings ++ ScalariformPlugin.defaultScalariformSettings ++ Seq(
    organization := "scalariform",
    version := "0.1.2-SNAPSHOT",
    scalaVersion := "2.9.2",
    crossScalaVersions := Seq("2.8.0", "2.8.1", "2.8.2", "2.9.0", "2.9.1", "2.9.2"),
    resolvers += ScalaToolsSnapshots,
    retrieveManaged := true,
    scalacOptions += "-deprecation",
    pomExtra := pomExtraXml,
    parallelExecution in Test := false,
    publishMavenStyle := true,
    publishArtifact in Test := false,
    pomIncludeRepository := { _ => false },
    EclipseKeys.withSource := true,
    EclipseKeys.eclipseOutput := Some("bin"))

  lazy val subprojectSettings = commonSettings ++ Seq(
    ScalariformKeys.preferences <<= baseDirectory.apply(dir ⇒ PreferencesImporterExporter.loadPreferences((dir / ".." / "formatterPreferences.properties").getPath)))

  lazy val root: Project = Project("root", file("."), settings = commonSettings) aggregate (scalariform, cli, misc)

  lazy val scalariform: Project = Project("scalariform", file("scalariform"), settings = subprojectSettings ++
    Seq(
      libraryDependencies <<= (scalaVersion, libraryDependencies) { (sv, deps) ⇒
        val scalatestVersion = sv match {
          case "2.8.0"           ⇒ "org.scalatest" %% "scalatest" % "1.3.1.RC2" % "test"
          case "2.10.0-M3"       ⇒ "org.scalatest" % "scalatest_2.10.0-M3" % "1.8-SNAPSHOT" % "test"
          case _                 ⇒ "org.scalatest" %% "scalatest" % "1.7.2" % "test"
        }
        deps :+ scalatestVersion
      },
      exportJars := true, // Needed for cli oneJar
      publishTo <<= version { (v: String) ⇒
        val nexus = "https://oss.sonatype.org/"
        if (v.trim.endsWith("SNAPSHOT")) 
          Some("snapshots" at nexus + "content/repositories/snapshots") 
        else
          Some("releases"  at nexus + "service/local/staging/deploy/maven2")
      }))

  lazy val cli = Project("cli", file("cli"), settings = subprojectSettings ++ SbtOneJar.oneJarSettings ++
    Seq(
      libraryDependencies += "commons-io" % "commons-io" % "1.4",
      mainClass in (Compile, packageBin) := Some("scalariform.commandline.Main"),
      artifactName in SbtOneJar.oneJar := { (config: String, module: ModuleID, artifact: Artifact) ⇒ "scalariform.jar" })) dependsOn (scalariform)

  lazy val misc: Project = Project("misc", file("misc"), settings = subprojectSettings ++
    Seq(
      libraryDependencies += "commons-io" % "commons-io" % "1.4",
      libraryDependencies += "com.miglayout" % "miglayout" % "3.7.4",
      mainClass in (Compile, run) := Some("scalariform.gui.Main"))) dependsOn (scalariform, cli)

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
    <scm>
      <url>git@github.com:mdr/scalariform.git</url>
      <connection>scm:git:git@github.com:mdr/scalariform</connection>
    </scm>
    <developers>
      <developer>
        <id>mdr</id>
        <name>Matt Russell</name>
        <url>https://github.com/mdr/</url>
      </developer>
    </developers>

}
