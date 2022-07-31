package com.anglypascal.mustache

import org.scalatest.flatspec.AnyFlatSpec

class DotNotationSpecification extends AnyFlatSpec:

  behavior of "mustache"

  it should "render {{.}} properly" in {
    var mustache1 = new Mustache("Hello,{{#name}} {{.}}{{/name}}!")
      .render(Map("name" -> List("world", "and", "everyone")))
      .toString
    assert(mustache1 === "Hello, world and everyone!")

    var mustache2 = new Mustache("Hello,{{#name}} {{.}}{{/name}}")
      .render(Map("name" -> List("world", "and", "everyone")))
      .toString
    assert(mustache2 === "Hello, world and everyone")
  }

  it should "find map value '.' first when rendering {{.}}" in {
    var mustache = new Mustache("Hello,{{#name}} {{.}}{{/name}}!")
      .render(Map("name" -> Map("." -> "world")))
      .toString
    assert(mustache === "Hello, world!")
  }
