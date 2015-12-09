package scalariform.gui

import java.awt.event._
import java.awt.{ List ⇒ _, _ }

import javax.swing._
import javax.swing.text._
import javax.swing.event._
import javax.swing.tree._
import javax.swing.table._
import javax.swing.border.TitledBorder

import net.miginfocom.layout._
import net.miginfocom.swing._

import scalariform.astselect._
import scalariform.utils.Utils._
import scalariform.utils.Range
import scalariform.parser._
import scalariform.formatter._
import scalariform.formatter.preferences._
import scalariform.lexer._
import scalariform.lexer.Tokens._
import scalariform.gui.SwingUtils._
import scalariform.parser._
import scala.util.parsing.input._
import scala.util.parsing.combinator._
import scala.util.Try

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

  private val showAstCheckBox = new JCheckBox("Show AST / Tokens")

  private val productionComboBox = new JComboBox(ProductionComboBoxModel)

  type TextStyle = Int

  def getHighlight(token: Token): (Color, TextStyle) =
    token.tokenType match {
      case STRING_LITERAL | STRING_PART | XML_ATTR_VALUE | XML_CDATA ⇒ (new Color(42, 0, 255), Font.PLAIN)
      case _ if token.isScalaDocComment ⇒ (new Color(63, 95, 191), Font.PLAIN)
      case LINE_COMMENT | MULTILINE_COMMENT | XML_COMMENT ⇒ (new Color(63, 127, 95), Font.PLAIN)
      case t if t.isKeyword || t.isXml ⇒ (new Color(127, 0, 85), Font.BOLD)
      case t if t.isLiteral ⇒ (new Color(0, 0, 192), Font.PLAIN)
      case _ ⇒ (Color.BLACK, Font.PLAIN)
    }

  def highlightToken(token: Token, document: StyledDocument) {
    if (token.tokenType == Tokens.EOF)
      return
    val style = document.addStyle("DUMMY", null)
    StyleConstants.setFontFamily(style, "monospaced")
    StyleConstants.setFontSize(style, 14)
    val (colour, fontStyle) = getHighlight(token)
    StyleConstants.setForeground(style, colour)
    StyleConstants.setBold(style, fontStyle == Font.BOLD)
    document.setCharacterAttributes(token.offset, token.length, style, true)
  }

  private val SCALA_VERSION = "2.10.0"

  private def syntaxHighlight(textPane: JTextPane) {
    if (!highlightCheckBox.isSelected)
      return
    val tokens = ScalaLexer.rawTokenise(textPane.getText, forgiveErrors = true, scalaVersion = SCALA_VERSION)
    val document = textPane.getStyledDocument
    for (token ← tokens)
      highlightToken(token, document)
  }

  private def highlightRedundantSemis(textPane: JTextPane) {
    val semis = RedundantSemicolonDetector.findRedundantSemis(textPane.getText, scalaVersion = SCALA_VERSION)
    val document = textPane.getStyledDocument
    for (semi ← semis) {
      val style = document.addStyle("semi", null)
      StyleConstants.setFontFamily(style, "monospaced")
      StyleConstants.setFontSize(style, 14)
      StyleConstants.setBackground(style, Color.YELLOW)
      document.setCharacterAttributes(semi.offset, semi.length, style, true)
    }
  }

  setLayout(new BorderLayout)

  setTitle("Scalariform " + scalariform.BuildInfo.version)

  val textFont = new Font("monospaced", Font.PLAIN, 14)

  val astTree = new JTree

  val tokensTable = new TokenTable
  val rawTokensTable = new TokenTable

  val outputTextPane = new JTextPane
  setFont(outputTextPane, textFont)
  outputTextPane.setEditable(false)

  var selectionStack: List[Range] = Nil

  val inputTextPane = new JTextPane
  inputTextPane.addKeyListener(new KeyAdapter {
    override def keyPressed(ev: KeyEvent) {
      if (ev.isAltDown && ev.isShiftDown && ev.getKeyCode == KeyEvent.VK_UP) {
        val caret = inputTextPane.getCaret
        val smallest = math.min(caret.getMark, caret.getDot)
        val largest = math.max(caret.getMark, caret.getDot)
        val selectedRange = Range(smallest, largest - smallest)
        AstSelector.expandSelection(inputTextPane.getText, selectedRange, scalaVersion = SCALA_VERSION) foreach {
          case Range(offset, length) ⇒
            selectionStack ::= selectedRange
            caret.setDot(offset)
            caret.moveDot(offset + length)
        }
      } else if (ev.isAltDown && ev.isShiftDown && ev.getKeyCode == KeyEvent.VK_DOWN)
        selectionStack match {
          case Range(offset, length) :: rest ⇒
            selectionStack = rest
            val caret = inputTextPane.getCaret
            caret.setDot(offset)
            caret.moveDot(offset + length)
          case _ ⇒
        }
    }
  })
  setFont(inputTextPane, textFont)
  def runFormatter() {
    try {
      onSwingThread {
        syntaxHighlight(inputTextPane)
        highlightRedundantSemis(inputTextPane)
        syntaxHighlight(outputTextPane)
      }

      val inputText = inputTextPane.getText

      val startTime = System.currentTimeMillis
      val outputText = try {
        specificFormatter.format(inputText, scalaVersion = SCALA_VERSION)(OptionsPanel.getFormattingPreferences)
      } catch {
        case e: RuntimeException ⇒
          if (showAstCheckBox.isSelected) {
            val tokens = ScalaLexer.tokenise(inputText, scalaVersion = SCALA_VERSION)
            val rawTokens = ScalaLexer.rawTokenise(inputText, scalaVersion = SCALA_VERSION)
            val tableModel = new TokenTableModel(tokens, FormatResult(Map(), Map(), Map()))
            tokensTable.setTokens(tokens)
            rawTokensTable.setTokens(rawTokens)
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
      val tokens = ScalaLexer.tokenise(inputText, scalaVersion = SCALA_VERSION)
      val rawTokens = ScalaLexer.rawTokenise(inputText, scalaVersion = SCALA_VERSION)
      val tokenCount = tokens.size
      setTitle("Scalariform " + scalariform.BuildInfo.version + " -- " + duration + "ms, " + tokenCount + " tokens, speed = " + (1000 * tokenCount / (duration + 1)) + " tokens/second")
      outputTextPane.setText(outputText)

      if (showAstCheckBox.isSelected) {
        val parseResult =
          try
            specificFormatter.parse(new ScalaParser(tokens.toArray))
          catch {
            case e: RuntimeException ⇒
              tokensTable.setTokens(tokens)
              rawTokensTable.setTokens(rawTokens)
              throw e
          }
        val treeModel = new ParseTreeModel(parseResult)
        astTree.setModel(treeModel)
        expandAll(astTree)

        val (outputText, formatResult) = specificFormatter.fullFormat(inputText, scalaVersion = SCALA_VERSION)(OptionsPanel.getFormattingPreferences)

        tokensTable.setTokens(tokens, formatResult)
        rawTokensTable.setTokens(rawTokens)
      }
    } catch {
      case t: Throwable ⇒
        outputTextPane.setText(t.toString + "\n" + t.getStackTrace.mkString("\n"))
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
  resultTabbedPane.addTab("Raw Tokens", new JScrollPane(rawTokensTable))

  val splitPane = new JSplitPane(JSplitPane.VERTICAL_SPLIT)
  splitPane.setTopComponent(new JScrollPane(inputTextPane))
  splitPane.setBottomComponent(resultTabbedPane)
  splitPane.setDividerLocation(0.5)
  splitPane.setResizeWeight(0.5)

  add(splitPane, BorderLayout.CENTER)

  object OptionsPanel extends JPanel(new MigLayout) {
    def addChangeListener(box: JToggleButton) {
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
          case IntentPreference ⇒
            val label = new JLabel(preference.description)
            val radioPanel = new JPanel(new GridLayout(0, 1));
            val radioGroup = new ButtonGroup()
            val radioForce = new JRadioButton("force")
            val radioPrevent = new JRadioButton("prevent")
            val radioPreserve = new JRadioButton("preserve")
            radioGroup.add(radioForce)
            radioGroup.add(radioPrevent)
            radioGroup.add(radioPreserve)

            radioPanel.add(label)
            radioPanel.add(radioForce)
            radioPanel.add(radioPrevent)
            radioPanel.add(radioPreserve)
            preference.defaultValue.asInstanceOf[Intent] match {
              case Force    ⇒ radioForce.setSelected(true)
              case Prevent  ⇒ radioPrevent.setSelected(true)
              case Preserve ⇒ radioPreserve.setSelected(true)
            }
            add(radioPanel, new CC().wrap)
            addChangeListener(radioForce)
            addChangeListener(radioPrevent)
            addChangeListener(radioPreserve)
            preferenceToWidgetMap += (preference -> radioPanel)
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
    add(new JScrollPane(PreferencesPanel), new CC().wrap)

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
          case prefType @ BooleanPreference ⇒
            val checkBox = widget.asInstanceOf[JCheckBox]
            preferences = preferences.setPreference(prefType.cast(preference), widget.asInstanceOf[JCheckBox].isSelected)
          case prefType @ IntegerPreference(min, max) ⇒
            preferences = preferences.setPreference(
              prefType.cast(preference),
              Integer.parseInt(widget.asInstanceOf[JSpinner].getValue.toString)
            )
          case prefType @ IntentPreference ⇒
            val selected = widget.asInstanceOf[JPanel].getComponents.find { c =>
              try {
                c.asInstanceOf[JRadioButton].isSelected
              } catch {
                case _: Throwable => false
              }
            }.get.asInstanceOf[JRadioButton].getText
            preferences = preferences.setPreference[Intent](
              prefType.cast(preference),
              IntentPreference.parseValue(selected).fold(
                err => throw new Exception(err),
                a => a
              )
            )
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
        range ← parseTreeModel.getDocumentRange(lastComponent)
      } {
        inputTextPane.setSelectionStart(range.offset)
        inputTextPane.setSelectionEnd(range.offset + range.length)
        inputTextPane.requestFocusInWindow()
        astTree.requestFocusInWindow()
      }

    }
  })

  tokensTable.getSelectionModel.addListSelectionListener { e: ListSelectionEvent ⇒
    for (token ← tokensTable.getSelectedToken) {
      val range = token.range
      inputTextPane.setSelectionStart(range.offset)
      inputTextPane.setSelectionEnd(range.offset + range.length)
      inputTextPane.requestFocusInWindow()
      tokensTable.requestFocusInWindow()
    }
  }

  rawTokensTable.getSelectionModel.addListSelectionListener { e: ListSelectionEvent ⇒
    for (token ← rawTokensTable.getSelectedToken) {
      val range = token.range
      inputTextPane.setSelectionStart(range.offset)
      inputTextPane.setSelectionEnd(range.offset + range.length)
      inputTextPane.requestFocusInWindow()
      rawTokensTable.requestFocusInWindow()
    }
  }

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

  def specificFormatter: SpecificFormatter =
    productionComboBox.getSelectedItem.asInstanceOf[ProductionItem].formatter

  class ProductionItem(name: String, val formatter: SpecificFormatter) {
    override def toString = name
  }

  object ProductionComboBoxModel extends DefaultComboBoxModel[ProductionItem] {

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
      new ProductionItem("Expression", expressionFormatter)
    )

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

