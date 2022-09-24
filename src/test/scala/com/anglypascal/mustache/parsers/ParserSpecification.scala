package com.anglypascal.mustache.parsers

import com.anglypascal.mustache.*
import org.scalatest.flatspec.AnyFlatSpec
import scala.io.Source

class ParserSpecification extends AnyFlatSpec:

  it should "handle static text only" in {
    var mustache = new Mustache("Hello, world!").render().toString
    assert(mustache === "Hello, world!")
  }

  it should "handle simple defues" in {
    def mus = new Mustache("Hello, {{name}}!")
    def map = Map("name" -> "world")
    assert(mus.render(map) === "Hello, world!")
  }

  it should "handle escaping properly" in {
    def mus = new Mustache("Hello, {{name}}!")
    def map = Map("name" -> "<world>")
    assert(mus.render(map) === "Hello, &lt;world&gt;!")
  }

  it should "handle unescaped tags" in {
    def mus = new Mustache("Hello, {{{name}}}!")
    def map = Map("name" -> "<world>")
    assert(mus.render(map) === "Hello, <world>!")
  }

  it should "handle unescaped tags with &" in {
    def mus = new Mustache("Hello, {{&name}}!")
    def map = Map("name" -> "<world>")
    assert(mus.render(map) === "Hello, <world>!")
  }

  it should "report error for unbalanced braces: {{{  }}}" in {
    def mus1 = new Mustache("{{{name}} ")
    assertThrows[MustacheParseException] { mus1 }

    def mus2 = new Mustache("{{{name}}")
    assertThrows[MustacheParseException] { mus2 }

    def mus3 = new Mustache("{{{}}")
    assertThrows[MustacheParseException] { mus3 }

    def mus4 = new Mustache("{{}")
    assertThrows[MustacheParseException] { mus4 }

    def mus5 = new Mustache("{{")
    assertThrows[MustacheParseException] { mus5 }
  }

  it should "ignore incomplete tags" in {
    def mus1 = new Mustache("{ {")
    assert(mus1.render() === "{ {")

    def mus2 = new Mustache("}} }")
    assert(mus2.render() === "}} }")
  }

  it should "report error for empty tag" in {
    def mus1 = new Mustache("{{{}}}")
    def mus2 = new Mustache("{{}}")
    assertThrows[MustacheParseException] { mus1 }
    assertThrows[MustacheParseException] { mus2 }
  }

  it should "handle boolean sections" in {
    def mus = new Mustache("Hello, {{#person}}{{name}}{{/person}}!")

    def map2 = Map("person" -> false, "name" -> "world")
    assert(mus.render(map2) === "Hello, !")

    def map3 = Map("person" -> true, "name" -> "world")
    assert(mus.render(map3) === "Hello, world!")
  }

  it should "handle nested sections" in {
    def mus1 = new Mustache("Hello, {{#person}}{{name}}{{/person}}!")
    def map1 = Map("person" -> Map("name" -> "world"))
    assert(mus1.render(map1) === "Hello, world!")

    def mus2 =
      new Mustache("Hello, {{#person}}{{#name}}{{foo}}{{/name}}{{/person}}!")
    def map2 = Map("person" -> Map("name" -> Map("foo" -> "world")))
    assert(mus2.render(map2) === "Hello, world!")
  }

  it should "report error for unclosed section" in {
    def mus1 = new Mustache("Hello, {{#person}}{{name}}!")
    assertThrows[MustacheParseException] { mus1 }
  }

  it should "report error for unclosed nested section" in {
    def mus1 = new Mustache("Hello, {{#person}}{{#name}}{{/person}}{{/name}}!")
    assertThrows[MustacheParseException] { mus1 }
  }

  it should "report error for invalid delimiter tag" in {
    def mus1 = new Mustache("{{=}}")
    assertThrows[MustacheParseException] { mus1 }
    def mus2 = new Mustache("{{==}}")
    assertThrows[MustacheParseException] { mus2 }
    def mus3 = new Mustache("{{= foo =}}")
    assertThrows[MustacheParseException] { mus3 }
    def mus4 = new Mustache("{{= foo }}")
    assertThrows[MustacheParseException] { mus4 }
  }

  it should "report error for invalid tags" in {
    def mus1 = new Mustache("{{>}}")
    assertThrows[MustacheParseException] { mus1 }
    def mus2 = new Mustache("{{<}}")
    assertThrows[MustacheParseException] { mus2 }
    def mus3 = new Mustache("{{&}}")
    assertThrows[MustacheParseException] { mus3 }
    def mus4 = new Mustache("{{^}} ... {{/}}")
    assertThrows[MustacheParseException] { mus4 }
    def mus5 = new Mustache("{{#}} ... {{/}}")
    assertThrows[MustacheParseException] { mus5 }
  }

  /** FIXME add column checker
    */
  it should "report lines properly" in {
    def errorLine(mus: => Mustache) =
      intercept[MustacheParseException] { mus }.row

    def errorCol(mus: => Mustache) =
      intercept[MustacheParseException] { mus }.col

    def mus1 = new Mustache("{{>}}")
    assert(errorLine(mus1) === 1)
    def mus2 = new Mustache("some text\n{{<}}")
    assert(errorLine(mus2) === 2)
    def mus3 = new Mustache("some text\r\n{{&}}")
    assert(errorLine(mus3) === 2)
    def mus4 = new Mustache("some text {{^}}\n... {{/}}")
    assert(errorLine(mus4) === 1)
    def mus5 = new Mustache("some text \n\r {{^}} ... {{/}}")
    assert(errorLine(mus5) === 3)
  }

  /** FIXME: Test if templateSource equals the input string. Also why are we
    * using Mustache here? Test Parser directly :/
    */

  it should "templateSource of RootToken should be equal to the source" in {
    val str =
      "hello {{world}} {{#this}}{{is}}{{/this}} a test {{&string}} {{=__ __=}}"
    val src = Source.fromString(str)
    val rt  = RecursiveParser.parse(src, "{{", "}}")
    assert(str === rt.templateSource)

  }
