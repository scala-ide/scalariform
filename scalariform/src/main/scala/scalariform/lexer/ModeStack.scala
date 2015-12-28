package scalariform.lexer

import scala.collection.mutable.Stack

/**
 * Keeping track of nesting level of XML within Scala.
 */
trait ModeStack { self: ScalaLexer ⇒

  private val modeStack = new Stack[LexerMode]

  modeStack.push(new ScalaMode)

  protected def popMode() {
    modeStack.pop()
  }

  protected def isRootMode = modeStack.size == 1

  protected def switchToScalaModeAndFetchToken() {
    switchToScalaMode()
    fetchScalaToken()
  }

  protected def switchToXmlModeAndFetchToken() {
    modeStack.push(new XmlMode)
    fetchXmlToken()
  }

  protected def switchToStringInterpolationMode(multiLine: Boolean) {
    modeStack.push(new StringInterpolationMode(multiLine))
  }

  protected def switchToScalaMode() {
    modeStack.push(new ScalaMode)
  }

  protected def isStringInterpolationMode = modeStack.head.isInstanceOf[StringInterpolationMode]

  protected def isXmlMode = modeStack.head.isInstanceOf[XmlMode]

  protected def isScalaMode = modeStack.head.isInstanceOf[ScalaMode]

  protected def xmlMode: XmlMode = modeStack.head.asInstanceOf[XmlMode]

  protected def scalaMode: ScalaMode = modeStack.head.asInstanceOf[ScalaMode]

  protected def stringInterpolationMode: StringInterpolationMode = modeStack.head.asInstanceOf[StringInterpolationMode]

}
