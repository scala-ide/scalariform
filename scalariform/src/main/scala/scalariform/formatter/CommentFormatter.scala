package scalariform.formatter

import scalariform.parser._
import scalariform.utils._
import scalariform.lexer._
import scalariform.formatter.preferences._
import scala.annotation.tailrec

trait CommentFormatter { self: HasFormattingPreferences with ScalaFormatter ⇒

  private def getLines(comment: String): (String, List[String]) = {
    val prefix = List("/** ", "/**", "/* ", "/*").find(comment.startsWith).get
    val (start, rest) = comment.splitAt(prefix.length)
    val (contents, _) = rest.splitAt(rest.length - "*/".length)
    val firstLine :: otherLines = contents.split("""\r?\n([ \t]*(\*(?!/))?)?""", Integer.MAX_VALUE).toList
    val afterStarSpaces = if (formattingPreferences(MultilineScaladocCommentsStartOnFirstLine)) 2 else 1
    val initialSpaces = firstLine takeWhile (_.isWhitespace)
    val adjustedLines = dropInitialSpaces(firstLine, initialSpaces.size) :: (otherLines map { dropInitialSpaces(_, afterStarSpaces) })
    //    val adjustedLines map { line ⇒ if (line startsWith "*/") "*" + line else line }
    (start, adjustedLines)
  }

  @tailrec
  private def dropInitialSpaces(s: String, maxSpacesToDrop: Int): String =
    if (maxSpacesToDrop > 0 && s.startsWith(" "))
      dropInitialSpaces(s drop 1, maxSpacesToDrop - 1)
    else
      s

  private def removeTrailingWhitespace(s: String) = s.reverse.dropWhile(_.isWhitespace).reverse

  private def pruneEmptyInitial(lines: List[String]) = lines match {
    case first :: rest if first.trim == "" ⇒ rest
    case _                                 ⇒ lines
  }

  private def pruneEmptyFinal(lines: List[String]) = pruneEmptyInitial(lines.reverse).reverse

  def formatComment(comment: HiddenToken, indentLevel: Int): String =
    if (comment.rawText contains '\n') {
      val sb = new StringBuilder
      val (start, rawLines) = getLines(comment.rawText)

      val lines = pruneEmptyFinal(pruneEmptyInitial(rawLines))

      val alignBeneathSecondAsterisk = formattingPreferences(PlaceScaladocAsterisksBeneathSecondAsterisk)
      val startOnFirstLine = formattingPreferences(MultilineScaladocCommentsStartOnFirstLine)
      val beforeStarSpaces = if (alignBeneathSecondAsterisk) "  " else " "
      val afterStarSpaces = if (startOnFirstLine && !alignBeneathSecondAsterisk) "  " else " "
      sb.append(start.trim)
      var firstLine = true
      for (line ← lines) {
        val trimmedLine = removeTrailingWhitespace(line)
        if (firstLine && startOnFirstLine) {
          if (trimmedLine.nonEmpty)
            sb.append(" ").append(trimmedLine)
        } else {
          sb.append(newlineSequence).indent(indentLevel).append(beforeStarSpaces).append("*")
          if (trimmedLine.nonEmpty)
            sb.append(afterStarSpaces).append(trimmedLine)
        }
        firstLine = false
      }
      sb.append(newlineSequence).indent(indentLevel).append(beforeStarSpaces).append("*/")
      sb.toString
    } else
      comment.rawText

}
