package com.anglypascal.mustache.tokens

import com.anglypascal.mustache._
import org.scalatest.flatspec.AnyFlatSpec

class EscapedTokenSpecification extends AnyFlatSpec:

  object SampleTemplate extends Mustache("")

  it should "escape characters properly" in {
    val et = EscapedToken("foo", "{{", "}}")
      .render(
        Map("foo" -> "\"<>&test\""),
        Map(),
        List(SampleTemplate)
      )
      .toString
    assert(et === "&quot;&lt;&gt;&amp;test&quot;")
  }
