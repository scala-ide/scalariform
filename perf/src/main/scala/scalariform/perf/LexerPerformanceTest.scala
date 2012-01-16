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
    val tokens = ScalaLexer.rawTokenise(source)
    println("Tokens: " + tokens.size)
    val ITERATIONS = 3000
    val WARMUP = 500
    1 to WARMUP foreach { _ ⇒ doIt(source) }

    val start = System.currentTimeMillis
    val durations = 1 to ITERATIONS map { _ =>
      val start1 = System.nanoTime
      doIt(source)
      val duration = System.nanoTime - start1
      duration.toDouble / 1000000.0
    }
    val duration = System.currentTimeMillis - start
    val meanT = duration.toDouble / ITERATIONS
    println("Raw average: " + meanT)
    def compute(iterable: Iterable[Double]): (Double, Double) = {
      def square(x: Double) = x * x
      val size = iterable.size
      val mean = durations.sum / size
      (mean, math.sqrt(durations.map(n => square(n - mean)).sum / size))
    }
    val (mean, stdDev) = compute(durations)
    def isNormal(d: Double) =  math.abs(d - mean) < 2 * stdDev
    val durations2 = durations.filter(isNormal)
    println("Original trials: " + ITERATIONS)
    println("Outliers removed: " + (ITERATIONS - durations2.size))
    val (mean2, stdDev2) = compute(durations2)
    println("Minimum: " + durations.min)
    println("Average: " + mean2 + " ms")
    println("Standard deviation: " + stdDev2 + " ms")
    println()
    durations.foreach(println)
  }

  private def doIt(s: String) = {
    //    UnicodeEscapeDecoder.decode(s)
    ScalaLexer.rawTokenise(s)
  }
  
}

