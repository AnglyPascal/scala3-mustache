package com.anglypascal.mustache.tokens

import org.scalatest.flatspec.AnyFlatSpec
import com.anglypascal.mustache.Mustache

class StaticTextTokenSpecification extends AnyFlatSpec:

  object S extends Mustache("")

  it should "render static text" in {
    val l = StaticTextToken("hello").render(null, Map(), List(S)).toString
    assert(l === "hello")
  }
