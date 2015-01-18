package com.danieltrinh.scalariform.gui

import javax.swing.event.TreeModelListener
import javax.swing.tree._
import com.danieltrinh.scalariform.lexer.Token
import com.danieltrinh.scalariform.parser.AstNode
import com.danieltrinh.scalariform.utils.Range

class ParseTreeModel(rootAstNode: AstNode) extends TreeModel {

  def getDocumentRange(obj: AnyRef): Option[Range] = obj.asInstanceOf[TreeNode].range

  abstract sealed class TreeNode(name: String) {

    def children: List[TreeNode]

    lazy val range: Option[Range] = {
      val childRanges = children.flatMap(_.range)
      if (childRanges.isEmpty)
        None
      else
        Some(childRanges.reduceLeft(_ mergeWith _))
    }

    override def toString = name

  }

  case class AstNodeNode(name: String, astNode: AstNode) extends TreeNode(name) {

    val fields = astNode.getFields

    lazy val children = fields flatMap {
      case (_, None) | (_, Nil) ⇒ None
      case (fieldName, value)   ⇒ Some(makeTreeNode(value.asInstanceOf[AnyRef], fieldName))
    }

    override def toString = {
      val typeName = astNode.getClass.getSimpleName
      (if (name != "") name + ": " else "") + typeName
    }

  }

  case class TokenNode(name: String, token: Token) extends TreeNode(name) {

    val children = Nil

    override lazy val range = Some(token.range)

    override def toString = name + ": " + token

  }

  case class ListNode(name: String, list: List[Any]) extends TreeNode(name) {

    lazy val children = list.zipWithIndex map { case (x, i) ⇒ makeTreeNode(x, i.toString) }

  }

  case class OptionNode(name: String, opt: Option[Any]) extends TreeNode(name) {

    lazy val children = opt map { x ⇒ makeTreeNode(x, "Some") } toList

  }

  case class EitherNode(name: String, either: Either[Any, Any]) extends TreeNode(name) {

    lazy val children = either match {
      case Left(obj)  ⇒ List(makeTreeNode(obj, "Left"))
      case Right(obj) ⇒ List(makeTreeNode(obj, "Right"))
    }

  }

  case class PairNode(name: String, pair: (Any, Any)) extends TreeNode(name) {

    lazy val children = List(makeTreeNode(pair._1, "_1"), makeTreeNode(pair._2, "_2"))

  }

  case class TodoNode(name: String, obj: Any) extends TreeNode(name) {

    val children = Nil

    override def toString = name + ": " + obj

  }

  def makeTreeNode(obj: Any, name: String = ""): TreeNode = obj match {
    case astNode: AstNode ⇒
      AstNodeNode(name, astNode)
    case token: Token ⇒
      TokenNode(name, token)
    case list: List[_] ⇒
      ListNode(name, list)
    case Some(x) ⇒
      makeTreeNode(x, name)
    case None ⇒
      OptionNode(name, None)
    case either: Either[_, _] ⇒
      EitherNode(name, either)
    case pair: (_, _) ⇒
      PairNode(name, pair)
    case _ ⇒
      TodoNode(name, obj)
  }

  lazy val getRoot = AstNodeNode("root", rootAstNode)

  def getChildCount(obj: AnyRef) = getChildren(obj).length

  def getChild(parent: AnyRef, index: Int): AnyRef = getChildren(parent)(index)

  def getIndexOfChild(parent: AnyRef, child: AnyRef) = getChildren(parent).indexOf(child)

  def isLeaf(obj: AnyRef): Boolean = getChildCount(obj) == 0

  def addTreeModelListener(l: TreeModelListener) {}

  def removeTreeModelListener(l: TreeModelListener) {}

  def valueForPathChanged(path: TreePath, newValue: Any) {}

  private def getChildren(obj: AnyRef) = obj.asInstanceOf[TreeNode].children

}

