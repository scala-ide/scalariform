package scalariform.forexpander

import scalariform.parser._
import scalariform.lexer.Tokens._
import scalariform.lexer._

sealed trait NscEnumerator
case class ValFrom(pat: Expr, rhs: Expr) extends NscEnumerator
case class ValEq(pat: Expr, rhs: Expr) extends NscEnumerator
case class Filter(test: Expr) extends NscEnumerator

object ForExpander {

  def expandFor(forExpr: ForExpr): CallExpr = {
    val (mapName, flatMapName) = if (forExpr.yieldOption.isDefined) ("map", "flatMap") else ("foreach", "foreach")
    makeFor(mapName, flatMapName, makeEnumerators(forExpr.enumerators), forExpr.body)
  }

  def makeEnumerators(enumerators: Enumerators): List[NscEnumerator] = {

    def makeEnumerators(enumerator: Enumerator): List[NscEnumerator] = enumerator match {
      case OldForGuard(expr)    ⇒ List(Filter(expr))
      case guard: Guard         ⇒ List(makeFilter(guard))
      case generator: Generator ⇒ enumeratorsFromGenerator(generator)
    }

    def enumeratorsFromGenerator(generator: Generator): List[NscEnumerator] = {
      val Generator(_, pattern, equalsOrArrowToken, expr, guards) = generator
      (generator.equalsOrArrowToken.tokenType match {
        case EQUALS ⇒ ValEq(pattern, expr)
        case LARROW ⇒ ValFrom(pattern, expr)
      }) :: guards.map(makeFilter)
    }

    makeEnumerators(enumerators.initialGenerator) ::: (enumerators.rest flatMap { case (_, enumerator) ⇒ makeEnumerators(enumerator) })
  }

  private def makeFilter(guard: Guard) = Filter(guard.expr)

  private def token(tokenType: TokenType, text: String) =
    Token(tokenType, text, 0, text.length - 1)

  private def makeFor(mapName: String, flatMapName: String, enums: List[NscEnumerator], body: Expr): CallExpr = {

    def makeCombination(meth: String, qual: Expr, pat: Expr, body: Expr) = {
      val argument = Argument(Expr(List(AnonymousFunction(List(pat), token(ARROW, "=>"), List(body)))))
      val arguments = ParenArgumentExprs(token(LPAREN, "("), List(argument), token(RPAREN, ")"))
      CallExpr(Some(List(ParenExpr(token(LPAREN, "("), List(qual), token(RPAREN, ")"))), token(DOT, ".")), token(VARID, meth), newLineOptsAndArgumentExprss = List((None, arguments)))
    }

    enums match {
      case ValFrom(pat, rhs) :: Nil ⇒
        makeCombination(mapName, rhs, pat, body)
      case ValFrom(pat, rhs) :: (rest@(ValFrom(_, _) :: _)) ⇒
        makeCombination(flatMapName, rhs, pat, Expr(List(makeFor(mapName, flatMapName, rest, body))))
      case ValFrom(pat, rhs) :: Filter(test) :: rest ⇒
        makeFor(mapName, flatMapName, ValFrom(pat, Expr(List(makeCombination("withFilter", rhs, pat, test)))) :: rest, body)
    }

  }

}
