package com.anglypascal.mustache

import org.scalatest.flatspec.AnyFlatSpec

class ValuesFormatterSpecification extends AnyFlatSpec:

  val T = ValuesFormatter

  it should "render empty strings for null and None" in {
    assert(T.format(null) === "")
    assert(T.format(None) === "")
  }

  it should "render Options properly" in {
    assert(T.format(Some("hello")) === "hello")
    assert(T.format(Some(Some("hello"))) === "hello")
  }

  it should "render everything else to strings properly" in {
    assert(T.format(40) === "40")
    assert(T.format(List(1, 2)) === "List(1, 2)")
    assert(T.format(false) === "false")
  }
