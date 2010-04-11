package scala.tools.eclipse.formatter

import org.eclipse.jface.action.IAction
import org.eclipse.ui.IEditorActionDelegate
import org.eclipse.ui.actions.ActionDelegate

import org.eclipse.ui.IEditorPart
import scala.tools.eclipse.ScalaSourceFileEditor
import scalariform.parser._
import scalariform.lexer._
import scalariform.formatter._
import scalariform.utils.TextEdit
import org.eclipse.text.undo.DocumentUndoManagerRegistry
import org.eclipse.text.edits.{ TextEdit ⇒ JFaceTextEdit, _ }
import org.eclipse.jface.text.TextUtilities

class FormatSourceAction extends ActionDelegate with IEditorActionDelegate {

  private var editorOption: Option[ScalaSourceFileEditor] = None

  override def run(action: IAction) {
    for (editor ← editorOption) {
      val document = editor.getDocumentProvider.getDocument(editor.getEditorInput)
      val source = document.get
      val lineDelimiter = Option(TextUtilities.getDefaultLineDelimiter(document))
      try {
        val edits = ScalaFormatter.formatAsEdits(source, FormatterPreferencePage.getPreferences, lineDelimiter)
        val eclipseEdit = new MultiTextEdit
        for (TextEdit(start, length, replacement) ← edits)
          eclipseEdit.addChild(new ReplaceEdit(start, length, replacement))
        val undoManager = DocumentUndoManagerRegistry.getDocumentUndoManager(document)
        undoManager.beginCompoundChange()
        new TextEditProcessor(document, eclipseEdit, JFaceTextEdit.NONE).performEdits
        undoManager.endCompoundChange()
      } catch {
        case _: ScalaParserException ⇒
        case e ⇒ throw e
      }
    }
  }

  def setActiveEditor(action: IAction, targetEditor: IEditorPart) {
    if (targetEditor.isInstanceOf[ScalaSourceFileEditor])
      editorOption = Some(targetEditor.asInstanceOf[ScalaSourceFileEditor])
  }

}
