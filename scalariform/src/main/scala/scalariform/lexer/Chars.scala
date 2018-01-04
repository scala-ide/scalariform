package scalariform.lexer

import scala.annotation.switch

object Chars {

  /**
   * @see scala.reflect.internal.Chars.isOperatorPart
   */
  def isOperatorPart(c: Char): Boolean =
    (c: @switch) match {
      case '~' | '!' | '@' | '#' | '%' |
        '^' | '*' | '+' | '-' | '<' |
        '>' | '?' | ':' | '=' | '&' |
        '|' | '/' | '\\' ⇒ true
      case _ ⇒ isSpecial(c)
    }

  /**
   * @see scala.reflect.internal.Chars.isSpecial
   */
  def isSpecial(c: Char): Boolean = {
    val chtp = Character.getType(c)
    chtp == Character.MATH_SYMBOL.toInt || chtp == Character.OTHER_SYMBOL.toInt
  }

  /**
   * @see scala.reflect.internal.Chars.isIdentifierStart
   */
  def isIdentifierStart(c: Char): Boolean =
    (c == '_') || (c == '$') || Character.isUnicodeIdentifierStart(c)

  /**
   * @see scala.reflect.internal.Chars.isIdentifierPart
   */
  def isIdentifierPart(c: Char): Boolean =
    (c == '$') || Character.isUnicodeIdentifierPart(c) && c != CharConstants.SU

}
