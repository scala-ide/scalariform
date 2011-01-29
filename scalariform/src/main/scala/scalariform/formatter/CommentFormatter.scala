package scalariform.formatter

import scalariform.parser._
import scalariform.utils._
import scalariform.lexer._
import scalariform.formatter.preferences._

trait CommentFormatter { self: HasFormattingPreferences with ScalaFormatter ⇒

  private def getLines(comment: String): (String, List[String]) = {
    val prefix = List("/** ", "/**", "/* ", "/*").find(comment.startsWith).get
    val (start, rest) = comment.splitAt(prefix.length)
    val (contents, _) = rest.splitAt(rest.length - "*/".length)
    val lines = contents.split("""\r?\n([ \t]*\*?[ \t]?)?""", Integer.MAX_VALUE).toList
    (start, lines)
  }

  private def removeTrailingWhitespace(s: String) = s.reverse.dropWhile(_.isWhitespace).reverse

  private def pruneEmptyInitial(lines: List[String]) = lines match {
    case first :: rest if first.trim == "" ⇒ rest
    case _                                 ⇒ lines
  }

  private def pruneEmptyFinal(lines: List[String]) = pruneEmptyInitial(lines.reverse).reverse

  def formatComment(comment: HiddenToken, indentLevel: Int): String =
    if (comment.text contains '\n') {
      val sb = new StringBuilder
      val (start, rawLines) = getLines(comment.text)

      val lines = pruneEmptyFinal(pruneEmptyInitial(rawLines))

      // println("formatComment: " + (start, lines))
      sb.append(start.trim)
      for (line ← lines) {
        val trimmedLine = removeTrailingWhitespace(line)
        sb.append(newlineSequence).indent(indentLevel).append(" *")
        if (!trimmedLine.isEmpty)
          sb.append(" ").append(trimmedLine)
      }
      sb.append(newlineSequence).indent(indentLevel).append(" */")
      sb.toString
    } else
      comment.text

}
