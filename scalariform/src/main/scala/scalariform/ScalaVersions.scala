package scalariform

import scala.util.Properties

/**
 * A group of Scala versions that Scalariform wants to distinguish because they have incompatible syntax
 */
sealed trait ScalaVersionGroup extends Ordered[ScalaVersionGroup] {

  def compare(that: ScalaVersionGroup) =
    this match {
      case `that`      ⇒ 0
      case SCALA_28_29 ⇒ -1
      case SCALA_211   ⇒ 1
    }

}

case object SCALA_28_29 extends ScalaVersionGroup
case object SCALA_210 extends ScalaVersionGroup
case object SCALA_211 extends ScalaVersionGroup

object ScalaVersions {

  def DEFAULT_GROUP = getVersionGroup(DEFAULT_VERSION)

  def DEFAULT_VERSION = Properties.scalaPropOrElse("version.number", "2.9.2")

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