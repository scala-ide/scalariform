package scalariform.lexer

import scala.collection.mutable.Stack

/**
 * Keeping track of nesting level of XML within Scala.
 */
trait ModeStack { self: ScalaLexer ⇒

  private val modeStack = new Stack[LexerMode]

  private var currentRegionStart: Int = 0

  modeStack.push(new ScalaMode)

  protected def popMode() {
    val mode = modeStack.pop()
  }

  protected def isRootMode = modeStack.size == 1

  protected def switchToScalaModeAndFetchToken() {
    modeStack.push(new ScalaMode)
    fetchScalaToken()
  }

  protected def switchToXmlModeAndFetchToken() {
    modeStack.push(new XmlMode)
    fetchXmlToken()
  }

  protected def isXmlMode = modeStack.head.isInstanceOf[XmlMode]

  protected def xmlMode: XmlMode = modeStack.head.asInstanceOf[XmlMode]

  protected def scalaMode: ScalaMode = modeStack.head.asInstanceOf[ScalaMode]

}