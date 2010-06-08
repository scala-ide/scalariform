package scalariform.lexer

import scalariform.parser.ScalaParserException

class ScalaLexerException(message: String) extends ScalaParserException(message) {}