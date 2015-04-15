package scalariform.lexer

sealed trait LexerMode

class ScalaMode extends LexerMode {

  private var braceNestLevel: Int = 0

  def nestBrace() { braceNestLevel += 1 }

  def unnestBrace(): Int = {
    braceNestLevel -= 1
    braceNestLevel
  }

}

class XmlMode extends LexerMode {

  var isTagMode: Boolean = false

  var tagState: TagState = Normal

  private var tagNestLevel: Int = 0

  def nestTag() { tagNestLevel += 1 }

  def unnestTag(): Int = {
    tagNestLevel -= 1
    tagNestLevel
  }

  def nestingLevel = tagNestLevel

}

class StringInterpolationMode(val multiLine: Boolean) extends LexerMode {

  var initialSegment = true

  var interpolationVariable = false

}
