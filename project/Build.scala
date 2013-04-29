import sbt._
import sbt.Keys._
import com.github.retronym.SbtOneJar
import com.typesafe.sbteclipse.core.EclipsePlugin.EclipseKeys._
import com.typesafe.sbteclipse.core.EclipsePlugin._
import com.typesafe.sbt.SbtScalariform
import com.typesafe.sbt.SbtScalariform.ScalariformKeys
import scalariform.formatter.preferences._

object ScalariformBuild extends Build {

  lazy val commonSettings = Defaults.defaultSettings ++ SbtScalariform.defaultScalariformSettings ++ Seq(
    organization := "org.scalariform",
    version := "0.1.5-SNAPSHOT",
    scalaVersion := "2.10.0",
    crossScalaVersions := Seq(
      //      "2.11.0-M2",
      "2.10.0", "2.10.1",
      "2.9.3", "2.9.2", "2.9.1-1", "2.9.1", "2.9.0-1", "2.9.0",
      "2.8.2", "2.8.1", "2.8.0"),
    exportJars := true, // Needed for cli oneJar
    retrieveManaged := true,
    scalacOptions += "-deprecation",
    EclipseKeys.withSource := true,
    EclipseKeys.eclipseOutput := Some("bin"))

  lazy val subprojectSettings = commonSettings ++ Seq(
    ScalariformKeys.preferences <<= baseDirectory.apply(getScalariformPreferences))

  def getScalariformPreferences(dir: File) =
    PreferencesImporterExporter.loadPreferences((dir / ".." / "formatterPreferences.properties").getPath)

  lazy val root: Project = Project("root", file("."), settings = commonSettings ++ Seq(
    publish := (),
    publishLocal := ())) aggregate (scalariform, cli, misc)

  def getScalaTestDependency(scalaVersion: String) = scalaVersion match {
    case "2.8.0"  ⇒ "org.scalatest" %% "scalatest" % "1.3.1.RC2" % "test"
    case "2.10.0" ⇒ "org.scalatest" %% "scalatest" % "1.9.1" % "test"
    case "2.10.1" ⇒ "org.scalatest" %% "scalatest" % "1.9.1" % "test"
    case "2.9.3"  ⇒ "org.scalatest" %% "scalatest" % "1.9.1" % "test"
    case _        ⇒ "org.scalatest" %% "scalatest" % "1.7.2" % "test"
  }

  lazy val scalariform: Project = Project("scalariform", file("scalariform"), settings =
    subprojectSettings ++ sbtbuildinfo.Plugin.buildInfoSettings ++ eclipseSettings ++
      Seq(
        libraryDependencies <<= (scalaVersion, libraryDependencies) { (sv, deps) ⇒ deps :+ getScalaTestDependency(sv) },
        pomExtra := pomExtraXml,
        publishMavenStyle := true,
        publishArtifact in Test := false,
        pomIncludeRepository := { _ ⇒ false },
        sbtbuildinfo.Plugin.buildInfoKeys := Seq[sbtbuildinfo.Plugin.BuildInfoKey](version),
        sbtbuildinfo.Plugin.buildInfoPackage := "scalariform",
        sourceGenerators in Compile <+= sbtbuildinfo.Plugin.buildInfo,
        EclipseKeys.createSrc := EclipseCreateSrc.Default + EclipseCreateSrc.Managed,
        publishTo <<= isSnapshot(getPublishToRepo)))

  def getPublishToRepo(isSnapshot: Boolean) =
    if (isSnapshot) Some("snapshots" at "https://oss.sonatype.org/content/repositories/snapshots")
    else Some("releases" at "https://oss.sonatype.org/service/local/staging/deploy/maven2")

  lazy val cli = Project("cli", file("cli"), settings = subprojectSettings ++ SbtOneJar.oneJarSettings ++
    Seq(
      libraryDependencies += "commons-io" % "commons-io" % "1.4",
      mainClass in (Compile, packageBin) := Some("scalariform.commandline.Main"),
      artifactName in SbtOneJar.oneJar := { (version: ScalaVersion, module: ModuleID, artifact: Artifact) ⇒ "scalariform.jar" },
      publish := (),
      publishLocal := ())) dependsOn (scalariform)

  lazy val misc: Project = Project("misc", file("misc"), settings = subprojectSettings ++
    Seq(
      libraryDependencies ++= Seq(
        "commons-io" % "commons-io" % "1.4",
        "com.miglayout" % "miglayout" % "3.7.4"),
      publish := (),
      publishLocal := (),
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
