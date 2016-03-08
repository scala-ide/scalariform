package scalariform.formatter

import scalariform.formatter.preferences._

// format: OFF
class RewriteArrowsTest extends AbstractExpressionFormatterTest {

  {
    implicit val formattingPreferences = FormattingPreferences.setPreference(RewriteArrowSymbols, true)

    "(a: Int) => 3" ==> "(a: Int) ⇒ 3"
    "for (i <- 1 to 10) yield i" ==> "for (i ← 1 to 10) yield i"
    // We don't rewrite RARROW anymore since it can be, and is, used as a
    // normal identifier.
    "cache += k -> f(k)" ==> "cache += k -> f(k)"
  }

  override val debug = false

}
