package scalariform.utils

import org.scalatest.{FlatSpec, Matchers}

// format: +preserveSpaceBeforeArguments
class TrimmedTest extends FlatSpec with Matchers {

  it should "just work" in {

    Trimmed.unapply("").get should equal (("", "", ""))
    Trimmed.unapply("foo").get should equal (("", "foo", ""))
    Trimmed.unapply("  foo").get should equal (("  ", "foo", ""))
    Trimmed.unapply("  foo  ").get should equal (("  ", "foo", "  "))
    Trimmed.unapply("  foo bar  ").get should equal (("  ", "foo bar", "  "))
    Trimmed.unapply(" ").get should equal ((" ", "", ""))

  }

}
