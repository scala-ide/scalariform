package scalariform.utils

case class TextEdit(position: Int, length: Int, replacement: String) {
  require(position >= 0)
  require(length >= 0)
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
