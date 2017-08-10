import com.typesafe.sbt.SbtScalariform.ScalariformKeys
import scalariform.formatter.preferences._

lazy val commonSettings = inConfig(Test)(Defaults.testSettings) ++
  SbtScalariform.defaultScalariformSettings ++ Seq(
    organization := "org.scalariform",
    sonatypeProfileName := organization.value,
    scalaVersion := crossScalaVersions.value.head,
    crossScalaVersions := Seq(
      "2.12.3",
      "2.11.11",
      "2.10.6"
    ),
    scalacOptions ++= (CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, 12)) => Seq(
        "-Xlint:-unused,_", "-Ywarn-unused:imports",
        "-language:postfixOps", "-language:implicitConversions",
        "-deprecation", "-feature"
      )
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
    case Some((2, major)) if major >= 11 => Seq(
      "org.scala-lang.modules" %% "scala-xml" % "1.0.6",
      "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.6"
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
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test",
    // sbt doesn't automatically load the content of the MANIFST.MF file, therefore
    // we have to do it here by ourselves Furthermore, the version format in the
    // MANIFEST.MF is `x.y.z.qualifier` but we need to replace the `qualifier` part
    // with a unique identifier otherwise OSGi can't find out which nightly build
    // is newest and therefore not all caches are updated with the correct version
    // of a nightly.
    packageOptions in Compile in packageBin += {
      val m = Using.fileInputStream(file("scalariform/META-INF/MANIFEST.MF")) { in =>
        val manifest = new java.util.jar.Manifest(in)
        val attr = manifest.getMainAttributes
        val key = "Bundle-Version"
        val versionSuffix = scalaBinaryVersion.value.replace('.', '_')
        val date = new java.text.SimpleDateFormat("yyyyMMddHHmm").format(new java.util.Date)
        val sha = "git rev-parse --short HEAD".!!.trim
        attr.putValue(key, attr.getValue(key).replace("qualifier", s"$versionSuffix-$date-$sha"))
        manifest
      }
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
  settings(publishSettings("root"))
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
    <developer>
      <id>machaval</id>
      <name>Mariano de Achaval</name>
      <url>https://github.com/machaval/</url>
    </developer>
    <developer>
      <id>godenji</id>
      <name>N.S. Cutler</name>
      <url>https://github.com/godenji/</url>
    </developer>
  </developers>
