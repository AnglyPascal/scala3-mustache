package com.anglypascal.mustache.tokens

import com.anglypascal.mustache._
import org.scalatest.flatspec.AnyFlatSpec

class UnescapedTokenSpecification extends AnyFlatSpec:

  object SampleTemplate extends Mustache("")

  it should "not escape any character" in {
    val et = UnescapedToken("foo", "{{", "}}")
      .render(
        Map("foo" -> "\"<>&test\""),
        Map(),
        List(SampleTemplate)
      )
      .toString
    assert(et === "\"<>&test\"")
  }


