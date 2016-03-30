import com.typesafe.sbt.SbtScalariform.ScalariformKeys
import scalariform.formatter.preferences._

lazy val commonSettings = inConfig(Test)(Defaults.testSettings) ++
    SbtScalariform.defaultScalariformSettings ++ Seq(
  organization := "org.scalariform",
  sonatypeProfileName := organization.value,
  scalaVersion := crossScalaVersions.value.head,
  crossScalaVersions := Seq(
    "2.11.8",
    "2.10.6"
  ),
  exportJars := true, // Needed for cli oneJar
  scalacOptions ++= (CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, major)) if major >= 11 =>
      scalac2_10Options ++ scalac2_11Options
    case _ =>
      scalac2_10Options
  }),
  credentials ++= {
    val creds = Path.userHome / ".m2" / "credentials"
    if (creds.exists) Seq(Credentials(creds)) else Nil
  }
)

def scalac2_10Options = Seq(
  "-encoding", "UTF-8",
  "-feature",
  "-language:_",
  "-unchecked",
  "-Xlint",
  "-Xfuture",
  "-Yno-adapted-args",
  "-Ywarn-dead-code"
)

def scalac2_11Options = Seq(
  "-deprecation:false",
  "-Xfatal-warnings",
  "-Ywarn-unused-import",
  "-Ywarn-unused"
)

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

def getPublishToRepo = Def.setting {
  if (isSnapshot.value)
    Some(Opts.resolver.sonatypeSnapshots)
  else
    Some(Opts.resolver.sonatypeStaging)
}

lazy val subprojectSettings = commonSettings :+ (
  ScalariformKeys.preferences := PreferencesImporterExporter.loadPreferences(
    (baseDirectory.value / ".." / "formatterPreferences.properties").getPath)
)

def scala2_11Dependencies = Def.setting {
  CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, 11)) => Seq(
      "org.scala-lang.modules" %% "scala-xml" % "1.0.5",
      "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4"
    )
    case _ => Nil
  }
}

lazy val scalariform = (project
  enablePlugins(BuildInfoPlugin)
  settings(subprojectSettings)
  settings(publishSettings("scalariform"))
  settings(
    libraryDependencies ++= scala2_11Dependencies.value,
    libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.6" % "test",
    // sbt doesn't automatically load the content of the MANIFEST.MF file, therefore we have to do it here by ourselves
    packageOptions in Compile in packageBin += {
      val m = Using.fileInputStream(file("scalariform/META-INF/MANIFEST.MF"))(in => new java.util.jar.Manifest(in))
      Package.JarManifest(m)
    },
    testOptions in Test += Tests.Argument("-oI")
  )
)

lazy val cli = (project
  enablePlugins(BuildInfoPlugin)
  settings(subprojectSettings)
  settings(publishSettings("cli"))
  settings(
    libraryDependencies += "commons-io" % "commons-io" % "1.4",
    mainClass in (Compile, packageBin) := Some("scalariform.commandline.Main"),
    mainClass in assembly := Some("scalariform.commandline.Main"),
    artifact in (Compile, assembly) := {
      val art = (artifact in (Compile, assembly)).value
      art.copy(`classifier` = Some("assembly"))
    }
  )
  settings(addArtifact(artifact in (Compile, assembly), assembly))
  dependsOn(scalariform)
)

lazy val root = (project in file(".")
  settings(commonSettings)
  aggregate(scalariform, cli)
)

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
