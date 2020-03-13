package scalariform.parser

class ScalaParserException(val message: String, val line: Int) extends RuntimeException(message)
