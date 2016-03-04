package scalariform.formatter

import scalariform.lexer._
import scalariform.formatter.preferences._
import scala.annotation.tailrec

trait CommentFormatter { self: HasFormattingPreferences with ScalaFormatter ⇒

  private def getLines(comment: String, afterStarSpaces: Int): (String, List[String]) = {
    val prefix = List("/** ", "/**", "/* ", "/*").find(comment.startsWith).get
    val (start, rest) = comment.splitAt(prefix.length)
    val (contents, _) = rest.splitAt(rest.length - "*/".length)
    val firstLine :: otherLines = contents.split("""\r?\n([ \t]*(\*(?!/))?)?""", Integer.MAX_VALUE).toList
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

  def formatScaladocComment(comment: HiddenToken, indentLevel: Int): String =
    if (comment.rawText contains '\n') {
      val alignBeneathSecondAsterisk = formattingPreferences(PlaceScaladocAsterisksBeneathSecondAsterisk)
      val startOnFirstLine = formattingPreferences(MultilineScaladocCommentsStartOnFirstLine)
      // Comments with right-justified asterisks always get one space. Left-justified asterisks get
      // two spaces only if they also start on the first line.
      val afterStarSpaces = if (alignBeneathSecondAsterisk || !startOnFirstLine) 1 else 2

      val (start, rawLines) = getLines(comment.rawText, afterStarSpaces)

      val lines = pruneEmptyFinal(pruneEmptyInitial(rawLines))

      val beforeStarSpacesString = if (alignBeneathSecondAsterisk) "  " else " "
      val afterStarSpacesString = " " * afterStarSpaces

      val sb = new StringBuilder(start.trim)
      var firstLine = true
      for (line ← lines) {
        val trimmedLine = removeTrailingWhitespace(line)
        if (firstLine && startOnFirstLine) {
          if (trimmedLine.nonEmpty)
            sb.append(" ").append(trimmedLine)
        } else {
          sb.append(newlineSequence).indent(indentLevel).append(beforeStarSpacesString).append("*")
          if (trimmedLine.nonEmpty)
            sb.append(afterStarSpacesString).append(trimmedLine)
        }
        firstLine = false
      }
      sb.append(newlineSequence).indent(indentLevel).append(beforeStarSpacesString).append("*/")
      sb.toString
    } else
      comment.rawText

  /** Formats a non-Scaladoc comment by trimming trailing whitespace from each line. */
  def formatNonScaladocComment(comment: HiddenToken, indentLevel: Int): String = {
    comment.rawText.replaceAll("""[ \t]+(\r?\n)""", "$1")
  }
}
