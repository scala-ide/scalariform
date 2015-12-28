package scalariform.utils

object TextEdit {

  def delete(range: Range): TextEdit = delete(range.offset, range.length)

  def delete(position: Int, length: Int): TextEdit = TextEdit(position = position, length = length, replacement = "")

}

case class TextEdit(position: Int, length: Int, replacement: String) {

  require(position >= 0, "position must be positive: " + position)

  require(length >= 0)

  override lazy val toString = {
    val replacementDisplay = replacement.replace("\n", """\n""").replace("\r", """\r""")
    getClass.getSimpleName + "(position = " + position + ", length = " + length + ", replacement = '" + replacementDisplay + "')"
  }

  def shift(n: Int) = copy(position = position + n)

}

object TextEditProcessor {

  /**
   * @param edits must be ordered and non-overlapping
   */
  def runEdits(s: String, edits: TextEdit*): String = runEdits(s, edits.toList)

  /**
   * @param edits must be ordered and non-overlapping
   */
  def runEdits(s: String, edits: List[TextEdit]): String = {
    val sb = new StringBuilder
    var pos = 0
    var editsRemaining = edits
    while (pos < s.length) {
      if (editsRemaining.isEmpty) {
        sb.append(s(pos))
        pos += 1
      } else {
        val edit = editsRemaining.head
        if (pos == edit.position) {
          editsRemaining = editsRemaining.tail
          pos += edit.length
          sb.append(edit.replacement)
        } else {
          sb.append(s(pos))
          pos += 1
        }
      }
    }
    var processEditsAtEnd = true
    while (processEditsAtEnd) {
      if (editsRemaining.isEmpty)
        processEditsAtEnd = false
      else {
        val edit = editsRemaining.head
        if (pos == edit.position) {
          editsRemaining = editsRemaining.tail
          pos += edit.length
          sb.append(edit.replacement)
        } else
          processEditsAtEnd = false
      }
    }
    require(editsRemaining.isEmpty)
    sb.toString
  }

}
