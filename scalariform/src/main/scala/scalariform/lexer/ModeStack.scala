package scalariform.lexer

import scala.collection.mutable

/**
 * Keeping track of nesting level of XML within Scala.
 */
trait ModeStack { self: ScalaLexer ⇒

  private val modeStack = new mutable.Stack[LexerMode]

  modeStack.push(new ScalaMode)

  protected def popMode(): Unit = {
    modeStack.pop()
  }

  protected def isRootMode: Boolean = modeStack.size == 1

  protected def switchToScalaModeAndFetchToken(): Unit = {
    switchToScalaMode()
    fetchScalaToken()
  }

  protected def switchToXmlModeAndFetchToken(): Unit = {
    modeStack.push(new XmlMode)
    fetchXmlToken()
  }

  protected def switchToStringInterpolationMode(multiLine: Boolean): Unit = {
    modeStack.push(new StringInterpolationMode(multiLine))
  }

  protected def switchToScalaMode(): Unit = {
    modeStack.push(new ScalaMode)
  }

  protected def isStringInterpolationMode: Boolean = modeStack.head.isInstanceOf[StringInterpolationMode]

  protected def isXmlMode: Boolean = modeStack.head.isInstanceOf[XmlMode]

  protected def isScalaMode: Boolean = modeStack.head.isInstanceOf[ScalaMode]

  protected def xmlMode: XmlMode = modeStack.head.asInstanceOf[XmlMode]

  protected def scalaMode: ScalaMode = modeStack.head.asInstanceOf[ScalaMode]

  protected def stringInterpolationMode: StringInterpolationMode = modeStack.head.asInstanceOf[StringInterpolationMode]

}
