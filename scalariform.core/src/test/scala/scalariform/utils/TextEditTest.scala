package scalariform.utils

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

// format: +preserveSpaceBeforeArguments
class TextEditTest extends FlatSpec with ShouldMatchers {

  import TextEditProcessor.runEdits

  it should "do replace edits" in {

    runEdits("012345", TextEdit(position = 0, length = 2, replacement = "wibble")) should equal ("wibble2345")

    runEdits("012345", TextEdit(position = 2, length = 2, replacement = "23")) should equal ("012345")

    runEdits("012345",
      TextEdit(position = 0, length = 2, replacement = "foo"),
      TextEdit(position = 3, length = 2, replacement = "bar")) should equal ("foo2bar5")
  }

  it should "do delete edits" in {

    runEdits("012345", TextEdit(position = 0, length = 1, replacement = "")) should equal ("12345")
    runEdits("012345", TextEdit(position = 0, length = 6, replacement = "")) should equal ("")
    runEdits("012345", TextEdit(position = 5, length = 1, replacement = "")) should equal ("01234")
    runEdits("012345", TextEdit(position = 1, length = 4, replacement = "")) should equal ("05")
    runEdits("0", TextEdit(position = 0, length = 1, replacement = "")) should equal ("")

    runEdits("012345",
      TextEdit(position = 0, length = 2, replacement = ""),
      TextEdit(position = 4, length = 2, replacement = "")) should equal ("23")

  }

  it should "do insert edits" in {

    runEdits("012345", TextEdit(position = 0, length = 0, replacement = "")) should equal ("012345")
    runEdits("012345", TextEdit(position = 6, length = 0, replacement = "")) should equal ("012345")
    runEdits("012345", TextEdit(position = 0, length = 0, replacement = "X")) should equal ("X012345")
    runEdits("012345", TextEdit(position = 1, length = 0, replacement = "X")) should equal ("0X12345")
    runEdits("012345", TextEdit(position = 5, length = 0, replacement = "X")) should equal ("01234X5")
    runEdits("012345", TextEdit(position = 6, length = 0, replacement = "X")) should equal ("012345X")
    runEdits("", TextEdit(position = 0, length = 0, replacement = "X")) should equal ("X")

    runEdits("012345",
      TextEdit(position = 0, length = 0, replacement = "X"),
      TextEdit(position = 3, length = 0, replacement = "X")) should equal ("X012X345")

  }

  it should "do multiple insert edits at same position" in {

    runEdits("012345",
      TextEdit(position = 0, length = 0, replacement = "X"),
      TextEdit(position = 0, length = 0, replacement = "Y")) should equal ("XY012345")

    runEdits("012345",
      TextEdit(position = 0, length = 0, replacement = "X"),
      TextEdit(position = 6, length = 0, replacement = "X"),
      TextEdit(position = 6, length = 0, replacement = "Y")) should equal ("X012345XY")

  }
}
