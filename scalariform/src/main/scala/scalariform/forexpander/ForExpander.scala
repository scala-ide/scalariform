package scalariform.forexpander

import scalariform.parser._
import scalariform.lexer.Tokens._
import scalariform.lexer._

sealed trait NscEnumerator
case class ValFrom(pat: Expr, rhs: Expr) extends NscEnumerator
case class ValEq(pat: Expr, rhs: Expr) extends NscEnumerator
case class Filter(test: Expr) extends NscEnumerator

object ForExpander {

  private val MaxTupleArity = 22

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

  private def makeBind(expr: Expr): Bind = expr match {
    case Expr(List(bind@Bind(_, _, _))) ⇒ bind
    case _                              ⇒ Bind(token(VARID, freshName()), token(AT, "@"), expr.contents)
  }

  private var n: Int = 1
  private def freshName(): String = {
    val name = "freshName" + n
    n += 1
    name
  }

  private def intersperse[A](sep: ⇒ A, c: List[A]): List[A] = c match {
    case Nil     ⇒ Nil
    case List(x) ⇒ List(x)
    case x :: xs ⇒ x :: sep :: intersperse(sep, xs)
  }

  private def comma = token(COMMA, ",")

  private def makeTuple(exprs: List[Expr]): Expr = {
    val contents = intersperse(GeneralTokens(List(comma)), exprs)
    Expr(List(ParenExpr(token(LPAREN, "("), contents, token(RPAREN, ")"))))
  }

  private def makeTupleTerm(uscoreOrIds: List[Token]): Expr = Expr(List(uscoreOrIds match {
    case List(uscoreOrId) ⇒
      GeneralTokens(List(uscoreOrId))
    case _ ⇒
      val contents = List(GeneralTokens(intersperse(comma, uscoreOrIds)))
      ParenExpr(token(LPAREN, "("), contents, token(RPAREN, ")"))
  }))

  private def makePatDef(pat: ExprElement, rhs: Expr): List[FullDefOrDcl] =
    List(FullDefOrDcl(Nil, Nil, PatDefOrDcl(token(VAL, "val"), pattern = Expr(List(pat)), otherPatterns = Nil, typedOpt = None, equalsClauseOption = Some(token(EQUALS, "="), rhs))))

  private def makeValue(bind: Bind): Token = bind.uscoreOrId

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
      case ValFrom(pat, rhs) :: rest ⇒
        val valeqs = rest.take(MaxTupleArity - 1).takeWhile(_.isInstanceOf[ValEq])
        val rest1 = rest.drop(valeqs.length)
        val pats = valeqs map { case ValEq(pat, _) ⇒ pat }
        val rhss = valeqs map { case ValEq(_, rhs) ⇒ rhs }

        val defpat1 = makeBind(pat)
        val defpats = pats map makeBind
        val pdefs: List[Stat] = (defpats, rhss).zipped flatMap makePatDef
        val ids = (defpat1 :: defpats) map makeValue

        val (firstStat :: otherStats) = pdefs :+ makeTupleTerm(ids)
        val statSeq = Right(StatSeq(None, Some(firstStat), otherStats map { stat ⇒ (token(SEMI, ";"), Some(stat)) }))
        val rhs1 = makeFor("map", "flatMap",
          List(ValFrom(Expr(List(defpat1)), rhs)),
          Expr(List(BlockExpr(token(LBRACE, "{"), statSeq, token(RBRACE, "}")))))

        val allpats = pat :: pats
        val vfrom1 = ValFrom(makeTuple(allpats), Expr(List(rhs1)))
        makeFor(mapName, flatMapName, vfrom1 :: rest1, body)
    }

  }

}
