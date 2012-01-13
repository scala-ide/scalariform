package scalariform.lexer

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.collection.JavaConversions._
import scalariform.utils.Utils._
import scalariform.lexer.Tokens._
import java.{ util ⇒ ju }

/**
 * Logic for promoting intertoken whitespace/comments to a NEWLINE or NEWLINES token as required.
 */
class NewlineInferencer(private val delegate: Iterator[(HiddenTokens, Token)]) extends HiddenTokenInfo {

  import NewlineInferencer._

  require(delegate.hasNext)

  private var hiddenPredecessors: ju.Map[Token, HiddenTokens] = new ju.HashMap()
  
  private var inferredNewlines: ju.Map[Token, HiddenTokens] = new ju.HashMap()

  def isInferredNewline(token: Token): Boolean = inferredNewlines containsKey token

  def inferredNewlines(token: Token): Option[HiddenTokens] = Option(inferredNewlines get token)

  def hiddenPredecessors(token: Token): HiddenTokens = hiddenPredecessors get token

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

  // Keeps track of the nested regions which allow multiple statements or not. An item in the stack indicates the
  // expected closing token type for that region.
  private var regionMarkerStack: List[TokenType] = Nil

  def nextToken(): Token = {
    val (token, hiddenTokens) = tokenToEmitNextTime match {
      case Some(token) ⇒
        tokenToEmitNextTime = None
        (token, HiddenTokens(Nil))
      case None ⇒
        fetchNextToken()
    }
    hiddenPredecessors.put(token, hiddenTokens)
    updateRegionStack(token.tokenType)
    previousTokenOption = Some(token)
    token
  }

  /**
   * Multiple statements are allowed immediately inside "{..}" regions, but disallowed immediately inside "[..]", "(..)"
   * and "case ... =>" regions.
   */
  private def updateRegionStack(tokenType: TokenType) =
    tokenType match {
      case LBRACE ⇒
        regionMarkerStack ::= RBRACE
      case LPAREN ⇒
        regionMarkerStack ::= RPAREN
      case LBRACKET ⇒
        regionMarkerStack ::= RBRACKET
      case CASE if !buffer.isEmpty ⇒
        val (_, followingToken) = buffer.front
        val followingTokenType = followingToken.tokenType
        // "case class" and "case object" are excluded from the usual "case .. =>" region.
        if (followingTokenType != CLASS && followingTokenType != OBJECT)
          regionMarkerStack ::= ARROW
      case tokenType if regionMarkerStack.headOption == Some(tokenType) ⇒
        regionMarkerStack = regionMarkerStack.tail
      case _ ⇒
    }

  private def fetchNextToken(): (Token, HiddenTokens) = {
    val (hiddenTokens, token) = consumeFromBuffer()
    if (shouldTranslateToNewline(hiddenTokens, token)) {
      val tokenType = if (hiddenTokens.text matches BLANK_LINE_PATTERN) NEWLINES else NEWLINE
      val newlineToken = Token(tokenType, hiddenTokens.text, hiddenTokens.offset, hiddenTokens.rawText)
      tokenToEmitNextTime = Some(token)
      inferredNewlines.put(newlineToken, hiddenTokens)
      (newlineToken, HiddenTokens(Nil))
    } else
      (token, hiddenTokens)
  }

  private def consumeFromBuffer(): (HiddenTokens, Token) = {
    val ((hiddenTokens, token), newBuffer) = buffer.dequeue
    buffer = newBuffer
    refillBuffer()
    require(bufferInvariant)
    (hiddenTokens, token)
  }

  private def shouldTranslateToNewline(hiddenTokens: HiddenTokens, followingToken: Token) = {
    val nextTokenType = followingToken.tokenType
    val nextCanBeginAStatement = !TOKENS_WHICH_CANNOT_BEGIN_A_STATEMENT(followingToken.tokenType) &&
      (nextTokenType == CASE implies followingTokenIsClassOrObject)
    val previousCanEndAStatement = previousTokenOption.map(_.tokenType).map(TOKENS_WHICH_CAN_END_A_STATEMENT).getOrElse(false)
    hiddenTokens.containsNewline && previousCanEndAStatement && nextCanBeginAStatement && multipleStatementsAllowed
  }

  private def multipleStatementsAllowed =
    regionMarkerStack.isEmpty || regionMarkerStack.head == RBRACE

  private def followingTokenIsClassOrObject: Boolean =
    buffer.headOption match {
      case None                      ⇒ false
      case Some((_, followingToken)) ⇒ followingToken.tokenType == CLASS || followingToken.tokenType == OBJECT
    }

}

object NewlineInferencer {

  private val TOKENS_WHICH_CAN_END_A_STATEMENT: Set[TokenType] = Set(
    INTEGER_LITERAL, FLOATING_POINT_LITERAL, CHARACTER_LITERAL, STRING_LITERAL, SYMBOL_LITERAL, VARID, OTHERID, PLUS,
    MINUS, STAR, PIPE, TILDE, EXCLAMATION, THIS, NULL, TRUE, FALSE, RETURN, TYPE, XML_EMPTY_CLOSE, XML_TAG_CLOSE,
    XML_COMMENT, XML_CDATA, XML_UNPARSED, XML_PROCESSING_INSTRUCTION, USCORE, RPAREN, RBRACKET, RBRACE)

  private val TOKENS_WHICH_CANNOT_BEGIN_A_STATEMENT: Set[TokenType] = Set(
    CATCH, ELSE, EXTENDS, FINALLY, FORSOME, MATCH, REQUIRES, WITH, YIELD, COMMA, DOT, SEMI, COLON, /* USCORE, */ EQUALS,
    ARROW, LARROW, SUBTYPE, VIEWBOUND, SUPERTYPE, HASH, LBRACKET, RPAREN, RBRACKET, RBRACE)

  private val BLANK_LINE_PATTERN = """(?s).*\n\s*\n.*"""

}
