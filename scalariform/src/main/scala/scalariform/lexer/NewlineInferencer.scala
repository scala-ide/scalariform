package scalariform.lexer

import scala.collection.immutable.Queue
import scalariform.utils.Utils.boolean2ImpliesWrapper
import scalariform.lexer.Tokens._
import scala.annotation.tailrec
import java.util.{ HashMap, Map ⇒ JMap }

class NewlineInferencer(private val delegate: Iterator[(HiddenTokens, Token)]) {
  import NewlineInferencer._

  require(delegate.hasNext)

  private var hiddenPredecessors: JMap[Token, HiddenTokens] = new HashMap()
  private var inferredNewlines: JMap[Token, HiddenTokens] = new HashMap()

  def isInferredNewline(token: Token): Boolean = inferredNewlines containsKey token

  def inferredNewlines(token: Token): Option[HiddenTokens] = Option(inferredNewlines get token)

  def hiddenPredecessors(token: Token): HiddenTokens = hiddenPredecessors get token

  import scala.collection.JavaConversions._
  lazy val allHiddenTokens = hiddenPredecessors.values ++ inferredNewlines.values

  private var buffer: Queue[(HiddenTokens, Token)] = Queue()
  @tailrec
  private def refillBuffer() {
    if (buffer.size < 2 && delegate.hasNext) {
      buffer = buffer enqueue delegate.next()
      refillBuffer()
    }
  }
  refillBuffer()

  private def bufferInvariant = buffer.size <= 2 && (delegate.hasNext implies buffer.size == 2)
  require(bufferInvariant)

  private var tokenToEmitNextTime: Option[Token] = None
  private var previousTokenOption: Option[Token] = None

  private var multipleStatementRegionMarkerStack: List[TokenType] = Nil
  private def multipleStatementsAllowed = multipleStatementRegionMarkerStack.isEmpty || multipleStatementRegionMarkerStack.head == RBRACE

  def nextToken(): Token = {
    val token = nextTokenCore()
    token.getType match {
      case LBRACE ⇒
        multipleStatementRegionMarkerStack ::= RBRACE
      case LPAREN ⇒
        multipleStatementRegionMarkerStack ::= RPAREN
      case LBRACKET ⇒
        multipleStatementRegionMarkerStack ::= RBRACKET
      case CASE if !buffer.isEmpty ⇒
        val followingTokenType = buffer.front._2.getType
        if (followingTokenType != CLASS && followingTokenType != OBJECT)
          multipleStatementRegionMarkerStack ::= ARROW
      case tokenType if multipleStatementRegionMarkerStack.headOption == Some(tokenType) ⇒
        multipleStatementRegionMarkerStack = multipleStatementRegionMarkerStack.tail
      case _ ⇒
    }
    previousTokenOption = Some(token)
    token
  }

  private def nextTokenCore(): Token = {
    def updatePredecessorsAndSuccesors(token: Token, hiddenTokens: HiddenTokens) = {
      hiddenPredecessors.put(token, hiddenTokens)
      token
    }
    tokenToEmitNextTime match {
      case Some(token) ⇒
        tokenToEmitNextTime = None
        updatePredecessorsAndSuccesors(token, new HiddenTokens(Nil))
      case None ⇒
        val (hiddenTokens, token) = consumeFromBuffer()
        hiddenTokens.newlines match {
          case Some(newlineToken) if shouldTranslateToNewline(nextToken = token) ⇒
            tokenToEmitNextTime = Some(token)
            inferredNewlines.put(newlineToken, hiddenTokens)
            updatePredecessorsAndSuccesors(newlineToken, new HiddenTokens(Nil))
          case _ ⇒
            updatePredecessorsAndSuccesors(token, hiddenTokens)
        }
    }
  }

  private def consumeFromBuffer(): (HiddenTokens, Token) = {
    val ((hiddenTokens, token), newBuffer) = buffer.dequeue
    buffer = newBuffer
    refillBuffer()
    require(bufferInvariant)
    (hiddenTokens, token)
  }

  private def shouldTranslateToNewline(nextToken: Token) = {
    val nextTokenType = nextToken.getType
    val nextCanBeginAStatement = !tokensWhichCannotBeginAStatement(nextToken.getType) && (nextTokenType == CASE implies followingTokenIsClassOrObject)
    val previousCanEndAStatement = previousTokenOption.map(_.getType).map(tokensWhichCanTerminateAStatement).getOrElse(false)
    previousCanEndAStatement && nextCanBeginAStatement && multipleStatementsAllowed
  }

  private def followingTokenIsClassOrObject: Boolean =
    buffer.headOption match {
      case None ⇒ false
      case Some((_, followingToken)) ⇒ followingToken.getType == CLASS || followingToken.getType == OBJECT
    }

}

object NewlineInferencer {

  val tokensWhichCanTerminateAStatement: Set[TokenType] = Set(
    INTEGER_LITERAL, FLOATING_POINT_LITERAL, CHARACTER_LITERAL, STRING_LITERAL, SYMBOL_LITERAL, VARID, OTHERID, PLUS, MINUS, STAR, PIPE, TILDE, EXCLAMATION,
    THIS, NULL, TRUE, FALSE, RETURN, TYPE, XML_EMPTY_CLOSE, XML_TAG_CLOSE, XML_COMMENT, XML_CDATA, XML_UNPARSED, XML_PROCESSING_INSTRUCTION,
    USCORE, RPAREN, RBRACKET, RBRACE)
  val tokensWhichCannotBeginAStatement: Set[TokenType] = Set(
    CATCH, ELSE, EXTENDS, FINALLY, FORSOME, MATCH, REQUIRES,
    WITH, YIELD, COMMA, DOT, SEMI, COLON, /* USCORE, */ EQUALS, ARROW, LARROW, SUBTYPE, VIEWBOUND,
    SUPERTYPE, HASH, LBRACKET, RPAREN, RBRACKET, RBRACE)

}
