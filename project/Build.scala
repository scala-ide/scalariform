import sbt._
import sbt.Keys._
import com.typesafe.sbt.SbtScalariform
import com.typesafe.sbt.SbtScalariform.ScalariformKeys
import scalariform.formatter.preferences._
import xerial.sbt.Sonatype._
import xerial.sbt.Sonatype.SonatypeKeys._
import sbtassembly.AssemblyPlugin.autoImport._
import sbtbuildinfo.BuildInfoPlugin.autoImport._

object ScalariformBuild extends Build {

  lazy val commonSettings = inConfig(Test)(Defaults.testSettings) ++ SbtScalariform.defaultScalariformSettings ++ sonatypeSettings ++ Seq(
    organization := "org.scalariform",
    sonatypeProfileName := organization.value,
    version := "0.2.0-SNAPSHOT",
    scalaVersion := crossScalaVersions.value.head,
    crossScalaVersions := Seq(
      "2.11.8",
      "2.10.6"
    ),
    exportJars := true, // Needed for cli oneJar
    scalacOptions ++= Seq(
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
    ),
    credentials ++= {
      val creds = Path.userHome / ".m2" / "credentials"
      if (creds.exists) Seq(Credentials(creds)) else Nil
    }
  )

  lazy val subprojectSettings = commonSettings :+ (
    ScalariformKeys.preferences := scalariformPreferences.value)

  def scalariformPreferences = Def.setting {
    PreferencesImporterExporter.loadPreferences((baseDirectory.value / ".." / "formatterPreferences.properties").getPath)
  }

  lazy val root = project in file(".") settings(commonSettings) aggregate(scalariform, cli)

  def getScalaTestDependency = "org.scalatest" %% "scalatest" % "2.2.6" % "test"

  def get2_11Dependencies = Def.setting {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, 11)) => Seq(
        "org.scala-lang.modules" %% "scala-xml" % "1.0.5",
        "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4"
      )
      case _ => Nil
    }
  }

  def publishSettings(projectName: String) = Seq(
    pomExtra := pomExtraXml,
    publishMavenStyle := true,
    publishArtifact in Test := false,
    publishArtifact in (Compile, packageDoc) := true,
    publishArtifact in (Compile, packageSrc) := true,
    pomIncludeRepository := { _ ⇒ false },
    buildInfoKeys := Seq[BuildInfoKey](version),
    buildInfoPackage := projectName,
    publishTo := getPublishToRepo.value
  )

  lazy val scalariform = project enablePlugins(sbtbuildinfo.BuildInfoPlugin) settings(
    subprojectSettings ++ publishSettings("scalariform") ++
      Seq(
        libraryDependencies ++= get2_11Dependencies.value :+ getScalaTestDependency,
        // sbt doesn't automatically load the content of the MANIFEST.MF file, therefore we have to do it here by ourselves
        packageOptions in Compile in packageBin += {
          val m = Using.fileInputStream(file("scalariform/META-INF/MANIFEST.MF"))(in => new java.util.jar.Manifest(in))
          Package.JarManifest(m)
        },
        testOptions in Test += Tests.Argument("-oI")))

  def getPublishToRepo = Def.setting {
    if (isSnapshot.value)
      Some(Opts.resolver.sonatypeSnapshots)
    else
      Some(Opts.resolver.sonatypeStaging)
  }

  lazy val cli = project enablePlugins(sbtbuildinfo.BuildInfoPlugin) settings(
    subprojectSettings ++ publishSettings("cli") ++
    Seq(
      libraryDependencies += "commons-io" % "commons-io" % "1.4",
      mainClass in (Compile, packageBin) := Some("scalariform.commandline.Main"),
      mainClass in assembly := Some("scalariform.commandline.Main"),
      artifact in (Compile, assembly) := {
        val art = (artifact in (Compile, assembly)).value
        art.copy(`classifier` = Some("assembly"))
      }
    ) ++ addArtifact(artifact in (Compile, assembly), assembly)
  ) dependsOn(scalariform)

  def pomExtraXml =
    <inceptionYear>2010</inceptionYear>
    <url>https://github.com/scala-ide/scalariform</url>
    <licenses>
      <license>
        <name>MIT License</name>
        <url>http://www.opensource.org/licenses/mit-license.php</url>
        <distribution>repo</distribution>
      </license>
    </licenses>
    <scm>
      <url>git@github.com:scala-ide/scalariform.git</url>
      <connection>scm:git:git@github.com:scala-ide/scalariform</connection>
    </scm>
    <developers>
      <developer>
        <id>scala-ide</id>
        <name>Scala IDE</name>
        <url>https://github.com/scala-ide/</url>
      </developer>
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
