package scalariform.utils

object BooleanLang {

  def not(b: Boolean) = !b

  class PimpedBoolean(b1: Boolean) {
    def and(b2: Boolean) = b1 && b2
    def or(b2: Boolean) = b1 || b2
  }

  implicit def boolean2PimpedBoolean(b: Boolean): PimpedBoolean = new PimpedBoolean(b)

}
