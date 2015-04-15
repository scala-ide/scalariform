package scalariform.lexer

abstract sealed trait TagState
case object InStartTag extends TagState
case object InEndTag extends TagState
case object Normal extends TagState

