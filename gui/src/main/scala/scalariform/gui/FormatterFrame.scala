package scalariform.gui

import java.awt.event._
import net.miginfocom.layout._
import net.miginfocom.swing._
import javax.swing._
import javax.swing.text._
import java.awt.event._
import java.awt.{ List ⇒ _, _ }

import javax.swing._
import javax.swing.event._
import javax.swing.text._
import javax.swing.tree._
import javax.swing.table._
import javax.swing.border.TitledBorder
import javax.swing.{ JMenu, JMenuItem, JMenuBar, SwingConstants }

import scalariform.utils.Utils._
import scalariform.parser._
import scalariform.formatter._
import scalariform.lexer._
import scalariform.lexer.Tokens._
import scalariform.formatter.preferences._

class FormatterFrame extends JFrame with SpecificFormatter {

  private val debugCheckBox = new JCheckBox("Debug")
  override def debug = debugCheckBox.isSelected

  private val highlightCheckBox = new JCheckBox("Use syntax highlighting", true)

  type Result = CompilationUnit

  def parse(parser: ScalaParser): Result = parser.compilationUnitOrScript

  def format(formatter: ScalaFormatter, result: Result) = formatter.format(result)(FormatterState(indentLevel = 0))

  private def setFont(textPane: JTextPane, font: Font) {
    val attrs = textPane.getInputAttributes
    StyleConstants.setFontFamily(attrs, font.getFamily)
    StyleConstants.setFontSize(attrs, font.getSize)
    val doc = textPane.getStyledDocument
    doc.setCharacterAttributes(0, doc.getLength + 1, attrs, false)
  }

  private val keywords = Set(ABSTRACT, CASE, CATCH, CLASS, DEF, DO,
    ELSE, EXTENDS, FINAL, FINALLY, FOR, FORSOME, IF, IMPLICIT, IMPORT,
    LAZY, MATCH, NEW, OBJECT, OVERRIDE, PACKAGE, PRIVATE, PROTECTED,
    REQUIRES, RETURN, SEALED, SUPER, THIS, THROW, TRAIT, TRY, TYPE, VAL,
    VAR, WHILE, WITH, YIELD, USCORE, COLON, EQUALS, ARROW, LARROW,
    SUBTYPE, VIEWBOUND, SUPERTYPE, HASH, AT, XML_ATTR_EQ,
    XML_START_OPEN, XML_EMPTY_CLOSE, XML_TAG_CLOSE, XML_END_OPEN)

  private val literals = Set(CHARACTER_LITERAL, INTEGER_LITERAL, FLOATING_POINT_LITERAL, SYMBOL_LITERAL, TRUE, FALSE, NULL)

  private val showAstCheckBox = new JCheckBox("Show AST / Tokens")

  private val productionComboBox = new JComboBox(ProductionComboBoxModel)

  type TextStyle = Int
  private def syntaxHighlight(textPane: JTextPane) {

    def getHighlight(token: Token): (Color, TextStyle) = {
      val tokenType = token.getType
      if (keywords.contains(tokenType))
        (new Color(127, 0, 85), Font.BOLD)
      else if (literals.contains(tokenType))
        (new Color(0, 0, 192), Font.PLAIN)
      else tokenType match {
        case STRING_LITERAL | XML_ATTR_VALUE | XML_CDATA ⇒ (new Color(42, 0, 255), Font.PLAIN)
        case MULTILINE_COMMENT if token.getText startsWith "/**" ⇒ (new Color(63, 95, 191), Font.PLAIN)
        case LINE_COMMENT | MULTILINE_COMMENT | XML_COMMENT ⇒ (new Color(63, 127, 95), Font.PLAIN)
        case _ ⇒ (Color.BLACK, Font.PLAIN)
      }
    }
    if (highlightCheckBox.isSelected) {
      val (lexer, tokens: List[Token]) = ScalaLexer.tokeniseFull(textPane.getText)

      val document = textPane.getStyledDocument
      def highlightToken(token: Token) {
        if (token.getType != Tokens.EOF) {
          val style = document.addStyle("DUMMY", null)
          StyleConstants.setFontFamily(style, "monospaced")
          StyleConstants.setFontSize(style, 14)
          val (colour, fontStyle) = getHighlight(token)
          StyleConstants.setForeground(style, colour)
          if (fontStyle == Font.BOLD)
            StyleConstants.setBold(style, true)
          else
            StyleConstants.setBold(style, false)
          val startIndex = token.getStartIndex
          val endIndex = token.getStopIndex
          document.setCharacterAttributes(startIndex, endIndex - startIndex + 1, style, true);
        }
      }

      for (token ← tokens) {
        highlightToken(token)
        for (hiddenToken ← lexer.hiddenPredecessors(token))
          highlightToken(hiddenToken.token)
        for (hiddenTokens ← lexer.inferredNewlines(token); hiddenToken ← hiddenTokens)
          highlightToken(hiddenToken.token)
      }
    }
  }

  setLayout(new BorderLayout)

  setTitle("Scalariform " + scalariform.VERSION)

  val textFont = new Font("monospaced", Font.PLAIN, 14)

  val astTree = new JTree
  val tokensTable = new JTable
  tokensTable.setCellSelectionEnabled(false)
  tokensTable.setRowSelectionAllowed(true)
  tokensTable.setColumnSelectionAllowed(false)

  val outputTextPane = new JTextPane
  setFont(outputTextPane, textFont)
  outputTextPane.setEditable(false)

  val inputTextPane = new JTextPane
  setFont(inputTextPane, textFont)
  def runFormatter() {
    try {
      val inputText = inputTextPane.getText

      val startTime = System.currentTimeMillis
      val outputText = try {
        specificFormatter.format(inputText)(OptionsPanel.getFormattingPreferences)
      } catch {
        case e: RuntimeException ⇒
          if (showAstCheckBox.isSelected) {
            val (lexer, tokens) = ScalaLexer.tokeniseFull(inputText)
            val tableModel = new TokenTableModel(tokens, FormatResult(Map(), Map(), Map()))
            tokensTable.setModel(tableModel)
            try {
              val parseResult = specificFormatter.parse(new ScalaParser(tokens.toArray))
              val treeModel = new ParseTreeModel(parseResult)
              astTree.setModel(treeModel)
              expandAll(astTree)
            } catch { case e: RuntimeException ⇒ }
          }
          throw e
      }
      val duration = System.currentTimeMillis - startTime
      val tokenCount = getTokens(inputText).size
      setTitle("Scalariform " + scalariform.VERSION + " -- " + duration + "ms, " + tokenCount + " tokens, speed = " + (1000 * tokenCount / (duration + 1)) + " tokens/second")
      outputTextPane.setText(outputText)
      onSwingThread {
        syntaxHighlight(inputTextPane)
        syntaxHighlight(outputTextPane)
      }

      if (showAstCheckBox.isSelected) {
        import scalariform.parser._
        import scala.util.parsing.input._
        import scala.util.parsing.combinator._

        val (lexer, tokens) = ScalaLexer.tokeniseFull(inputText)
        val parseResult = try {
          specificFormatter.parse(new ScalaParser(tokens.toArray))
        } catch {
          case e: RuntimeException ⇒
            val tableModel = new TokenTableModel(tokens, FormatResult(Map(), Map(), Map()))
            tokensTable.setModel(tableModel)
            throw e
        }
        val treeModel = new ParseTreeModel(parseResult)
        astTree.setModel(treeModel)
        expandAll(astTree)

        val (outputText, formatResult) = specificFormatter.fullFormat(inputText)(OptionsPanel.getFormattingPreferences)

        val tableModel = new TokenTableModel(tokens, formatResult)
        tokensTable.setModel(tableModel)
      }
    } catch {
      case e ⇒
        outputTextPane.setText(e.toString + "\n" + e.getStackTrace.mkString("\n"))
        outputTextPane.setCaretPosition(0)
    }

  }
  inputTextPane.getDocument().addDocumentListener(new DocumentListener() {
    def changedUpdate(e: DocumentEvent) {}
    def insertUpdate(e: DocumentEvent) { runFormatter() }
    def removeUpdate(e: DocumentEvent) { runFormatter() }
  })

  val resultTabbedPane = new JTabbedPane()
  resultTabbedPane.addTab("Output", new JScrollPane(outputTextPane))
  resultTabbedPane.addTab("AST", new JScrollPane(astTree))
  resultTabbedPane.addTab("Tokens", new JScrollPane(tokensTable))

  val splitPane = new JSplitPane(JSplitPane.VERTICAL_SPLIT)
  splitPane.setTopComponent(new JScrollPane(inputTextPane))
  splitPane.setBottomComponent(resultTabbedPane)
  splitPane.setDividerLocation(0.5)
  splitPane.setResizeWeight(0.5)

  add(splitPane, BorderLayout.CENTER)

  object OptionsPanel extends JPanel(new MigLayout) {
    def addChangeListener(box: JCheckBox) {
      box.addItemListener(new ItemListener() {
        def itemStateChanged(e: ItemEvent) {
          runFormatter()
        }
      })
    }

    var preferenceToWidgetMap: Map[PreferenceDescriptor[_], JComponent] = Map()
    object PreferencesPanel extends JPanel(new MigLayout) {
      setBorder(new TitledBorder("Preferences"))
      for (preference ← AllPreferences.preferences) {
        val preferenceType = preference.preferenceType
        preferenceType match {
          case BooleanPreference ⇒
            val checkBox = new JCheckBox(preference.description)
            checkBox.setSelected(preference.defaultValue.asInstanceOf[Boolean])
            add(checkBox, new CC().wrap)
            addChangeListener(checkBox)
            preferenceToWidgetMap += (preference -> checkBox)
          case IntegerPreference(min, max) ⇒
            val label = new JLabel(preference.description)
            add(label, new CC)
            val spinner = new JSpinner
            spinner.setValue(preference.defaultValue.asInstanceOf[Int])
            spinner.addChangeListener(new ChangeListener() { def stateChanged(e: ChangeEvent) { runFormatter() } })
            add(spinner, new CC().wrap)
            preferenceToWidgetMap += (preference -> spinner)
        }
      }
    }
    add(PreferencesPanel, new CC().wrap())

    val makeTestButton = new JButton("Make test")
    makeTestButton.addActionListener(new ActionListener() {
      def actionPerformed(e: ActionEvent) {
        println()
        println("  \"\"\"" + (inputTextPane.getText.split("\n") mkString "\n    |") + "\"\"\" ==>")
        println("  \"\"\"" + (outputTextPane.getText.split("\n") mkString "\n    |") + "\"\"\"")
      }
    })
    add(makeTestButton, new CC().wrap())

    add(showAstCheckBox, new CC().wrap())
    addChangeListener(showAstCheckBox)

    add(debugCheckBox, new CC().wrap())
    addChangeListener(debugCheckBox)

    add(highlightCheckBox, new CC().wrap())
    addChangeListener(highlightCheckBox)

    add(new JLabel("Parser:"), new CC().wrap())
    add(productionComboBox, new CC().wrap())
    productionComboBox.setSelectedIndex(0)
    productionComboBox.addActionListener(new ActionListener() { def actionPerformed(e: ActionEvent) { runFormatter() } })

    def getFormattingPreferences = {

      var preferences: IFormattingPreferences = FormattingPreferences()

      for (preference ← AllPreferences.preferences) {
        val widget = preferenceToWidgetMap(preference)
        preference.preferenceType match {
          case prefType@BooleanPreference ⇒
            val checkBox = widget.asInstanceOf[JCheckBox]
            preferences = preferences.setPreference(prefType.cast(preference), widget.asInstanceOf[JCheckBox].isSelected)
          case prefType@IntegerPreference(min, max) ⇒
            preferences = preferences.setPreference(prefType.cast(preference), Integer.parseInt(widget.asInstanceOf[JSpinner].getValue.toString))
        }
      }
      preferences

    }

  }
  add(OptionsPanel, BorderLayout.EAST)

  astTree.addTreeSelectionListener(new TreeSelectionListener() {
    def valueChanged(e: TreeSelectionEvent) {
      val parseTreeModel = astTree.getModel.asInstanceOf[ParseTreeModel]
      for {
        lastComponent ← Option(astTree.getLastSelectedPathComponent)
        (from, to) ← parseTreeModel.getDocumentRange(lastComponent)
      } {
        inputTextPane.setSelectionStart(from)
        inputTextPane.setSelectionEnd(to + 1)
      }

    }
  })
  tokensTable.getSelectionModel.addListSelectionListener(new ListSelectionListener() {
    def valueChanged(e: ListSelectionEvent) {
      val tableModel = tokensTable.getModel.asInstanceOf[TokenTableModel]
      val selectionIndex = e.getFirstIndex
      if (selectionIndex >= 0) {
        val (from, to) = tableModel.getDocumentRange(selectionIndex)
        inputTextPane.setSelectionStart(from)
        inputTextPane.setSelectionEnd(to + 1)
      }

    }
  })

  {
    val menuBar = new JMenuBar
    val applicationMenu = new JMenu("Application")
    val exitMenuItem = new JMenuItem("Exit", KeyEvent.VK_X)
    exitMenuItem.addActionListener(new ActionListener() {
      def actionPerformed(e: ActionEvent) {
        FormatterFrame.this.dispose()
      }
    })
    applicationMenu.add(exitMenuItem)
    menuBar.add(applicationMenu)
    val samplesMenu = new JMenu("Samples")
    val sample1Item = new JMenuItem("Sample 1")
    sample1Item.addActionListener(new ActionListener() {
      def actionPerformed(e: ActionEvent) {
        inputTextPane.setText(Samples.sample1)
      }
    })
    samplesMenu.add(sample1Item)
    menuBar.add(samplesMenu)
    setJMenuBar(menuBar)
    inputTextPane.setText("")
  }

  def specificFormatter = productionComboBox.getSelectedItem.asInstanceOf[ProductionComboBoxModel.ProductionItem].formatter

  object ProductionComboBoxModel extends DefaultComboBoxModel {

    class ProductionItem(name: String, val formatter: SpecificFormatter) {
      override def toString = name
    }

    val compilationUnitFormatter = new SpecificFormatter {

      type Result = CompilationUnit

      def parse(parser: ScalaParser): Result = parser.compilationUnitOrScript()

      def format(formatter: ScalaFormatter, result: Result) = formatter.format(result)(FormatterState(indentLevel = 0))

    }

    val expressionFormatter = new SpecificFormatter {

      type Result = Expr

      def parse(parser: ScalaParser): Result = parser.expr() // TODO: EOF?

      def format(formatter: ScalaFormatter, result: Result) = formatter.format(result)(FormatterState(indentLevel = 0))

      override def debug = FormatterFrame.this.debug

    }

    val productions = List(
      new ProductionItem("Compilation unit or script", compilationUnitFormatter),
      new ProductionItem("Expression", expressionFormatter))

    override def getSize = productions.size
    override def getElementAt(index: Int) = productions(index)

  }

}

// format: OFF
object Samples {

  val sample0 =
  """class A {
	|
	|}""".stripMargin

  val sample1 =
  """package  foo . bar . baz
    |  /** 
    |  * Scaladoc comment
    |   *
    |*/
    | class A(n : Int) extends B  {
    |
    |println( "wibble")
    |
    |}""".stripMargin
} // format: ON

class TokenTableModel(tokens: List[Token], formatResult: FormatResult) extends AbstractTableModel {
  def getColumnCount = 5
  def getRowCount = tokens.size
  def getValueAt(row: Int, col: Int): AnyRef = {
    val token = tokens(row)
    col match {
      case 0 ⇒ token.tokenType
      case 1 ⇒ token.text
      case 2 ⇒ token.startIndex.asInstanceOf[java.lang.Integer]
      case 3 ⇒ token.stopIndex.asInstanceOf[java.lang.Integer]
      case 4 ⇒ {
        formatResult.predecessorFormatting.get(token) orElse formatResult.inferredNewlineFormatting.get(token) getOrElse ""
      }
    }
  }
  override def getColumnName(col: Int) = col match {
    case 0 ⇒ "Type"
    case 1 ⇒ "Token text"
    case 2 ⇒ "Start"
    case 3 ⇒ "Finish"
    case 4 ⇒ "Instruction"
  }

  def getDocumentRange(index: Int): (Int, Int) = {
    val token = tokens(index)
    (token.getStartIndex, token.getStopIndex)
  }
}

