package scalariform.perf

import scalariform.lexer.{ Token ⇒ _, _ }
import scalariform.utils.Utils.time
import java.io.File
import scala.io.Source

object LexerPerformanceTest extends Application {

  val file = new File("/home/matt/corpus2/" + "scala/src/compiler/scala/tools/nsc/symtab/Types.scala")
  val source = Source.fromFile(file).mkString
  if (true) {
    1 to 10 foreach { _ ⇒ ScalaLexer.tokeniseFull(source) }

    val (_, tokens) = ScalaLexer.tokeniseFull(source)
    println(file + " -- " + tokens.length + " tokens")

    time("Tokenise") {
      1 to 100 foreach { _ ⇒ ScalaLexer.tokeniseFull(source) }
    }
  } else {
    1 to 10 foreach { _ ⇒ ScalaLexer.rawTokenise2(source) }

    val tokens = ScalaLexer.rawTokenise2(source)
    println(file + " -- " + tokens.length + " tokens")

    time("Tokenise") {
      1 to 100 foreach { _ ⇒ ScalaLexer.rawTokenise2(source) }
    }
  }
  // 22706ms / 100, 21829ms
  // 20070ms ,19879ms ==> switch to Chars
  // 16454ms, 16449 => unicodeescapereader not backed by a Reader
  // 18113ms => post EOF SU fix
  // 16956ms, after removal of string builder
  // 12760ms => Maps => java.util.HashMap
}

