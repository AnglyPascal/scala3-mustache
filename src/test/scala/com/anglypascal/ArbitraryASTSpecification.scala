package com.anglypascal.mustache

import org.scalatest.flatspec.AnyFlatSpec

class ArbitraryASTSpecification extends AnyFlatSpec:

  trait JSON

  case class JStr(get: String) extends JSON with AST:
    def findKey(key: String): Option[Any] = Some(get)
    def value: Any                        = get

  case class JObj(get: Map[String, Any]) extends JSON with AST:
    def findKey(key: String): Option[Any] = get.get(key)
    def value: Any                        = get

  case class JArr(get: List[Any]) extends JSON with AST:
    def findKey(key: String): Option[Any] = Some(get)
    def value: Any                        = get

  val json1 = JObj(Map("name" -> JStr("world")))
  val json2 = JObj(
    Map("name" -> JArr(List(Map("a" -> JStr("b")), Map("a" -> JStr("c")))))
  )

  it should "handle static text only" in {
    var mustache1 = new Mustache("Hello, {{name}}!").render(json1).toString
    var mustache2 =
      new Mustache("Hello, {{#name}}{{a}}{{/name}}!").render(json2).toString
    assert(mustache1 === "Hello, world!")
    assert(mustache2 === "Hello, bc!")
  }
