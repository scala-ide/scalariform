package scalariform.gui

import scalariform.formatter.FormatResult
import javax.swing.JTable
import scalariform.lexer.Token
import javax.swing.table.AbstractTableModel
import scalariform.utils.Range

class TokenTable extends JTable(new TokenTableModel(Nil, FormatResult.EMPTY)) {

  setCellSelectionEnabled(false)
  setRowSelectionAllowed(true)
  setColumnSelectionAllowed(false)

  def setTokens(tokens: List[Token], formatResult: FormatResult = FormatResult.EMPTY) {
    val tableModel = new TokenTableModel(tokens, formatResult)
    setModel(tableModel)
  }

  override def getModel = super.getModel.asInstanceOf[TokenTableModel]

  def getSelectedToken: Option[Token] =
    getSelectedRow() match {
      case -1 ⇒ None
      case n  ⇒ Some(getModel.tokens(n))
    }

}

class TokenTableModel(val tokens: List[Token], formatResult: FormatResult) extends AbstractTableModel {

  def getColumnCount = 5

  def getRowCount = tokens.size

  def getValueAt(row: Int, col: Int): AnyRef = {
    val token = tokens(row)
    col match {
      case 0 ⇒ token.tokenType
      case 1 ⇒ token.text
      case 2 ⇒ token.offset.asInstanceOf[java.lang.Integer]
      case 3 ⇒ token.lastCharacterOffset.asInstanceOf[java.lang.Integer]
      case 4 ⇒ formatResult.predecessorFormatting.get(token) orElse formatResult.inferredNewlineFormatting.get(token) getOrElse ""
    }
  }

  override def getColumnName(col: Int) = col match {
    case 0 ⇒ "Type"
    case 1 ⇒ "Token text"
    case 2 ⇒ "Start"
    case 3 ⇒ "Finish"
    case 4 ⇒ "Instruction"
  }

}