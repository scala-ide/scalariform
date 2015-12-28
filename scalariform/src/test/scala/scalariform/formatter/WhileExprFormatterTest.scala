package scalariform.formatter

import scalariform.parser._
import scalariform.formatter._

// format: OFF
class WhileExprFormatterTest extends AbstractExpressionFormatterTest {

  "while( true )  run()" ==> "while (true) run()"

  """while( true )
    |run()""" ==>
  """while (true)
    |  run()"""

  "while (true) { run() }" ==> "while (true) { run() }"

  """while (true) { 
    |run() }""" ==>
  """while (true) {
    |  run()
    |}"""

  "do run()while( true )" ==> "do run() while (true)"

  """do 
    |run() while (true)""" ==>
  """do
    |  run()
    |while (true)"""

  """do {
    |run()
    |}
    |while (true)""" ==>
  """do {
    |  run()
    |} while (true)"""

  """do {
    |run()
    |};
    |while (true)""" ==>
  """do {
    |  run()
    |}; while (true)"""

  """do run();
    |while (true)""" ==>
  """do run();
    |while (true)"""

  """do run()
    |while (true)""" ==>
  """do run()
    |while (true)"""

  """do 
    |run()
    |while (true)""" ==>
  """do
    |  run()
    |while (true)"""

  """do 
    |run()
    |;
    |while (true)""" ==>
  """do
    |  run();
    |while (true)"""

  """do { run() };
    |while (true)""" ==>
  """do { run() };
    |while (true)"""

  """do { run() }
    |while (true)""" ==>
  """do { run() }
    |while (true)"""

  """while(true){do { run(); };
    |while (if (true) {
    |a} else { 
    |b})
    |}""" ==>
  """while (true) {
    |  do { run(); };
    |  while (if (true) {
    |    a
    |  } else {
    |    b
    |  })
    |}"""

  """a(while (b) 
    |c())""" ==>
  """a(while (b)
    |  c())"""

}

 
