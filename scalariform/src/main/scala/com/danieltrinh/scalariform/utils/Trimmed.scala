package scalariform.utils

object Trimmed {

  def apply(prefix: String, infix: String, suffix: String): String = prefix + infix + suffix

  def unapply(s: String): Option[(String, String, String)] = {
    val (prefix, rest) = s span Character.isWhitespace
    val (revSuffix, revInfix) = rest.reverse span Character.isWhitespace
    Some(prefix, revInfix.reverse, revSuffix.reverse)
  }

}
