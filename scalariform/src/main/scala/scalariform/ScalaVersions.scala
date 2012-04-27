package scalariform

import scala.util.Properties

/**
 * A group of Scala versions that Scalariform wants to distinguish because they have incompatible syntax
 */
sealed class ScalaVersionGroup(protected[scalariform] val internalVersion: Int) extends Ordered[ScalaVersionGroup] {
  def compare(that: ScalaVersionGroup) = internalVersion compare that.internalVersion
}

case object SCALA_28_29 extends ScalaVersionGroup(0)
case object SCALA_210 extends ScalaVersionGroup(1)
case object SCALA_211 extends ScalaVersionGroup(2)

object ScalaVersions {

  lazy val DEFAULT_GROUP = getVersionGroup(DEFAULT_VERSION)

  lazy val DEFAULT_VERSION = Properties.scalaPropOrElse("version.number", "2.9.2")

  def getVersionGroup(version: String): ScalaVersionGroup =
    version match {
      case _ if version startsWith "2.8."  ⇒ SCALA_28_29
      case _ if version startsWith "2.9."  ⇒ SCALA_28_29
      case _ if version startsWith "2.10." ⇒ SCALA_210
      case _                               ⇒ SCALA_211
    }

  def representativeVersion(versionGroup: ScalaVersionGroup) = versionGroup match {
    case SCALA_28_29 ⇒ "2.9.1"
    case SCALA_210   ⇒ "2.10.0"
    case SCALA_211   ⇒ "2.11.0"
  }

}