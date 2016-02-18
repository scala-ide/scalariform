import sbt._
import sbt.Keys._
import com.typesafe.sbt.SbtScalariform
import com.typesafe.sbt.SbtScalariform.ScalariformKeys
import scalariform.formatter.preferences._
import xerial.sbt.Sonatype._
import xerial.sbt.Sonatype.SonatypeKeys._
import sbtassembly.AssemblyPlugin.autoImport._

object ScalariformBuild extends Build {

  lazy val commonSettings = inConfig(Test)(Defaults.testSettings) ++ SbtScalariform.defaultScalariformSettings ++ sonatypeSettings ++ Seq(
    organization := "org.scalariform",
    profileName := "org.scalariform",
    version := "0.2.0-SNAPSHOT",
    scalaVersion := "2.11.7",
    crossScalaVersions := Seq(
      "2.11.7",
      "2.10.6",
      "2.9.3", "2.9.2" //"2.9.1-1", "2.9.1", "2.9.0-1", "2.9.0"
    ),
    exportJars := true, // Needed for cli oneJar
    scalacOptions ++= (scalaBinaryVersion.value match {
      case "2.11" => Seq(
        "-deprecation:false",
        "-encoding", "UTF-8",
        "-feature",
        "-language:_",
        "-unchecked",
        "-Xlint",
        "-Xfuture",
        "-Xfatal-warnings",
        "-Yno-adapted-args",
        "-Ywarn-dead-code",
        "-Ywarn-unused-import",
        "-Ywarn-unused"
      )
      case _ => Seq()
    })
  )

  lazy val subprojectSettings = commonSettings ++ Seq(
    ScalariformKeys.preferences <<= baseDirectory.apply(getScalariformPreferences))

  def getScalariformPreferences(dir: File) =
    PreferencesImporterExporter.loadPreferences((dir / ".." / "formatterPreferences.properties").getPath)

  lazy val root: Project = Project("root", file("."), settings = commonSettings) aggregate (scalariform, cli)

  implicit class Regex(sc: StringContext) {
    def r = new util.matching.Regex(sc.parts.mkString, sc.parts.tail.map(_ => "x"): _*)
  }

  def getScalaTestDependency(scalaVersion: String) = scalaVersion match {
    case r"2.11.\d+[-\w]*" | r"2.10.\d+[-\w]*" => "org.scalatest" %%  "scalatest" % "2.2.4" % "test"
    case "2.9.3" => "org.scalatest" %% "scalatest" % "1.9.1" % "test"
    case _       => "org.scalatest" %% "scalatest" % "1.7.2" % "test"
  }

  def get2_11Dependencies(scalaVersion: String): List[ModuleID] = scalaVersion match {
    case r"2.11.\d+[-\w]*" => List(
      "org.scala-lang.modules" %% "scala-xml" % "1.0.1",
      "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.1"
    )
    case _ => Nil
  }

  def publishSettings(projectName: String) = Seq(
    pomExtra := pomExtraXml,
    publishMavenStyle := true,
    publishArtifact in Test := false,
    publishArtifact in (Compile, packageDoc) := true,
    publishArtifact in (Compile, packageSrc) := true,
    pomIncludeRepository := { _ ⇒ false },
    sbtbuildinfo.Plugin.buildInfoKeys := Seq[sbtbuildinfo.Plugin.BuildInfoKey](version),
    sourceGenerators in Compile <+= sbtbuildinfo.Plugin.buildInfo,
    sbtbuildinfo.Plugin.buildInfoPackage := projectName
  )

  lazy val scalariform: Project = Project("scalariform", file("scalariform"), settings =
    subprojectSettings ++ sbtbuildinfo.Plugin.buildInfoSettings ++ publishSettings("scalariform") ++
      Seq(
        libraryDependencies <<= (scalaVersion, libraryDependencies) { (sv, deps) ⇒
          deps ++ get2_11Dependencies(sv) :+ getScalaTestDependency(sv)
        },
        // sbt doesn't automatically load the content of the MANIFST.MF file, therefore we have to do it here by ourselves
        packageOptions in Compile in packageBin += {
          val m = Using.fileInputStream(new java.io.File("scalariform/META-INF/MANIFEST.MF"))(in => new java.util.jar.Manifest(in))
          Package.JarManifest(m)
        },
        testOptions in Test += Tests.Argument("-oI"),
        publishTo <<= isSnapshot(getPublishToRepo)))

  def getPublishToRepo(isSnapshot: Boolean) =
    if (isSnapshot)
      Some("snapshots" at "https://oss.sonatype.org/content/repositories/snapshots")
    else
      Some("releases" at "https://oss.sonatype.org/service/local/staging/deploy/maven2")

  lazy val cli = Project("cli", file("cli"), settings = subprojectSettings ++ publishSettings("cli") ++
    sbtbuildinfo.Plugin.buildInfoSettings ++
    Seq(
      libraryDependencies += "commons-io" % "commons-io" % "1.4",
      mainClass in (Compile, packageBin) := Some("scalariform.commandline.Main"),
      mainClass in assembly := Some("scalariform.commandline.Main"),
      publishTo <<= isSnapshot(getPublishToRepo),
      artifact in (Compile, assembly) := {
        val art = (artifact in (Compile, assembly)).value
        art.copy(`classifier` = Some("assembly"))
      }
    ) ++ addArtifact(artifact in (Compile, assembly), assembly)
  ) dependsOn (scalariform)

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
      <url>git@github.com:daniel-trinh/scalariform.git</url>
      <connection>scm:git:git@github.com:daniel-trinh/scalariform</connection>
    </scm>
    <developers>
      <developer>
        <id>mdr</id>
        <name>Matt Russell</name>
        <url>https://github.com/mdr/</url>
      </developer>
      <developer>
        <id>daniel-trinh</id>
        <name>Daniel Trinh</name>
        <url>https://github.com/daniel-trinh/</url>
      </developer>
    </developers>

}
