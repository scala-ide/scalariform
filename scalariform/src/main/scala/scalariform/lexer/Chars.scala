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
      case c ⇒ isSpecial(c)
    }

  /**
   * @see scala.reflect.internal.Chars.isSpecial
   */
  def isSpecial(c: Char) = {
    val chtp = Character.getType(c)
    chtp == Character.MATH_SYMBOL.toInt || chtp == Character.OTHER_SYMBOL.toInt
  }

  /**
   * @see scala.reflect.internal.Chars.isIdentifierStart
   */
  def isIdentifierStart(c: Char) =
    (c == '_') || (c == '$') || Character.isUnicodeIdentifierStart(c)

  /**
   * @see scala.reflect.internal.Chars.isIdentifierPart
   */
  def isIdentifierPart(c: Char) =
    (c == '$') || Character.isUnicodeIdentifierPart(c) && c != CharConstants.SU

}
