package scalariform.perf

import java.io.File
import scala.io.Source
import scalariform.parser._
import scalariform.lexer.{ Token ⇒ _, _ }
import scalariform.utils.Utils.time
import scalariform.formatter._

object LexerPerformanceTest {

  def main(args: Array[String]) {

    val file = new File("/home/matt/coding/scala/src/compiler/scala/tools/nsc/typechecker/Typers.scala")
    val source = Source.fromFile(file).mkString
    println("Source: " + source.length + " chars")
    1 to 1000 foreach { _ ⇒ doIt(source) }

    val its = 10000
    
    val start = System.currentTimeMillis
      1 to its foreach { _ => doIt(source) }
    val duration = System.currentTimeMillis - start
    println(duration.toDouble / its + " ms")
    
  }

  private def unicodeEscapeReader(s: String) = new UnicodeEscapeReader(s)

  private def doIt(s: String) = {
    UnicodeEscapeDecoder.decode(s)
  }
}

