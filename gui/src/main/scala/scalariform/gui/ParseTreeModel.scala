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

import javax.swing.{ JMenu, JMenuItem, JMenuBar }

import scalariform.utils.Utils._
import scalariform.parser._
import scalariform.formatter._
import scalariform.lexer._
import scalariform.lexer.Tokens._

class ParseTreeModel(rootAstNode: AstNode) extends TreeModel {

  def getDocumentRange(obj: AnyRef): Option[(Int, Int)] = obj.asInstanceOf[TreeNode].range

  abstract sealed class TreeNode(name: String) {
    def children: List[TreeNode]
    override def toString = name
    lazy val range: Option[(Int, Int)] = {
      val childRanges = children flatMap { _.range }
      for {
        firstRange ← childRanges.headOption
        lastRange ← childRanges.lastOption
      } yield (firstRange._1, lastRange._2)
    }

  }

  case class AstNodeNode(name: String, astNode: AstNode) extends TreeNode(name) {
    val fields = astNode.getFields
    lazy val children = fields flatMap {
      case (_, None) | (_, Nil) ⇒ None
      case (fieldName, value) ⇒ Some(makeTreeNode(value.asInstanceOf[AnyRef], fieldName))
    }
    override def toString = {
      val typeName = astNode.getClass.getSimpleName
      (if (name != "") name + ": " else "") + typeName
    }
  }

  case class TokenNode(name: String, token: Token) extends TreeNode(name) {
    val children = Nil
    override def toString = name + ": " + token

    override lazy val range = Some(token.getStartIndex, token.getStopIndex)
  }
  case class ListNode(name: String, list: List[_ <: AnyRef]) extends TreeNode(name) {
    lazy val children = list map { makeTreeNode(_) }
  }
  case class OptionNode(name: String, opt: Option[_ <: AnyRef]) extends TreeNode(name) {
    lazy val children = opt map { makeTreeNode(_) } toList
  }

  case class EitherNode(name: String, either: Either[_ <: AnyRef, _ <: AnyRef]) extends TreeNode(name) {
    lazy val children = either match {
      case Left(obj) ⇒ List(makeTreeNode(obj))
      case Right(obj) ⇒ List(makeTreeNode(obj))
    }

  }

  case class PairNode(name: String, pair: (_ <: AnyRef, _ <: AnyRef)) extends TreeNode(name) {
    lazy val children = List(makeTreeNode(pair._1), makeTreeNode(pair._2))
  }

  case class TodoNode(name: String, obj: AnyRef) extends TreeNode(name) {
    val children = Nil
    override def toString = name + ": " + obj
  }

  def makeTreeNode(obj: AnyRef, name: String = ""): TreeNode = {
    for (astNode ← obj.matchInstance[AstNode])
      return AstNodeNode(name, astNode)
    for (token ← obj.matchInstance[Token])
      return TokenNode(name, token)
    for (list ← obj.matchInstance[List[_ <: AnyRef]])
      return ListNode(name, list)
    for (option ← obj.matchInstance[Option[_ <: AnyRef]])
      return OptionNode(name, option)
    for (either ← obj.matchInstance[Either[_ <: AnyRef, _ <: AnyRef]])
      return EitherNode(name, either)
    for (pair ← obj.matchInstance[(_ <: AnyRef, _ <: AnyRef)])
      return PairNode(name, pair)
    return TodoNode(name, obj)
  }

  lazy val getRoot = AstNodeNode("root", rootAstNode)

  def getChildCount(obj: AnyRef) = getChildren(obj).length

  private def getChildren(obj: AnyRef) = obj.asInstanceOf[TreeNode].children

  def getChild(parent: AnyRef, index: Int): AnyRef = getChildren(parent)(index)

  def getIndexOfChild(parent: AnyRef, child: AnyRef) = getChildren(parent).indexOf(child)

  def isLeaf(obj: AnyRef): Boolean = getChildCount(obj) == 0

  def addTreeModelListener(l: TreeModelListener) {}
  def removeTreeModelListener(l: TreeModelListener) {}
  def valueForPathChanged(path: TreePath, newValue: Any) {}

}

