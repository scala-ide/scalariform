package scalariform

import scala.math.Ordering
import scala.util.Properties
import scalariform.utils.Utils._

object ScalaVersion {

  private val VersionPattern = """(\d+)\.(\d+)\.(.*)""".r

  def parseOrDefault(s: String): ScalaVersion = parse(s).getOrElse(ScalaVersions.DEFAULT)

  def parse(s: String): Option[ScalaVersion] =
    s match {
      case VersionPattern(majorStr, minorStr, extra) ⇒
        for {
          major ← majorStr.toIntOpt
          minor ← minorStr.toIntOpt
        } yield ScalaVersion(major, minor, extra)
      case _ ⇒
        None
    }

}

case class ScalaVersion(major: Int, minor: Int, extra: String = "") extends Ordered[ScalaVersion] {

  private def majorMinor = (major, minor)

  def compare(that: ScalaVersion): Int = Ordering[(Int, Int)].compare(this.majorMinor, that.majorMinor)

  override def toString: String = major + "." + minor + "." + extra

}

object ScalaVersions {

  val Scala_2_11: ScalaVersion = ScalaVersion.parse("2.11.0").get
  val Scala_2_10: ScalaVersion = ScalaVersion.parse("2.10.0").get
  val Scala_2_9: ScalaVersion = ScalaVersion.parse("2.9.2").get
  val Scala_2_8: ScalaVersion = ScalaVersion.parse("2.8.1").get

  lazy val DEFAULT_VERSION: String = Properties.scalaPropOrElse("version.number", "2.9.2")

  lazy val DEFAULT: ScalaVersion = ScalaVersion.parse(DEFAULT_VERSION).get

}
