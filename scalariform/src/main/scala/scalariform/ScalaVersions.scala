package scalariform

/**
 * A group of Scala versions that Scalariform wants to distinguish (primarily because they have incompatible syntax)
 */
sealed trait ScalaVersionGroup
case object SCALA_28_29_210 extends ScalaVersionGroup
case object SCALA_211 extends ScalaVersionGroup

object ScalaVersions {
  
  def DEFAULT_VERSION = "2.9.1"

  def getVersionGroup(version: String): ScalaVersionGroup =
    version match {
      case _ if version startsWith "2.8."  ⇒ SCALA_28_29_210
      case _ if version startsWith "2.9."  ⇒ SCALA_28_29_210
      case _ if version startsWith "2.10." ⇒ SCALA_28_29_210
      case _                               ⇒ SCALA_211
    }

  def representativeVersion(versionGroup: ScalaVersionGroup) = versionGroup match {
    case SCALA_28_29_210 => "2.9.1"
    case SCALA_211 => "2.11"
  }
  
}