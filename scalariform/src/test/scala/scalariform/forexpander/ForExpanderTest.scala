package scalariform.forexpander

import scalariform.lexer._
import scalariform.parser._
import scalariform.formatter._

import org.scalatest._
import org.scalatest.matchers._

import scalariform.utils.Range

// format: OAF
class ForExpanderTest extends FlatSpec with ShouldMatchers {

//  private val source = "for (x <- 1 to 10; y <- 1 to 10 if y > x) yield x * y"
  private val source = "for (x <- 1 to 10; z = x * x) yield z"
  private val (hiddenTokenInfo, tokens) = ScalaLexer.tokeniseFull(source)
  private val parser = new ScalaParser(tokens.toArray)
  private val Expr(List(forExpr: ForExpr)) = parser.safeParse(parser.expr).get

  val expandedFor = ForExpander.expandFor(forExpr)

  println(expandedFor)
  println()

  expandedFor.tokens foreach println

  val rawText = expandedFor.tokens.map(_.text).mkString(" ")
  println(rawText)
  val result = ScalaFormatter.format(rawText)
  println(source)
  println(result)

}
