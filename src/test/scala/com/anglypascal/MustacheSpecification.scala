package com.anglypascal.mustache

import org.scalatest.flatspec.AnyFlatSpec

class MustacheSpecification extends AnyFlatSpec:

  behavior of "mustache: dot notation"

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

  behavior of "mustache: lambdas"

  it should "handle () => String functions" in {
    val mus = new Mustache("{{value}}")
    val map = Map("value" -> (() => { "hey" }))
    assert(mus.render(map) === "hey")
  }

  it should "handle String => String functions" in {
    val mus = new Mustache("{{#bold}}hey{{/bold}}")
    val map = Map("bold" -> ((str: String) => { "<b>" + str + "</b>" }))
    assert(mus.render(map) === "<b>hey</b>")
  }

  it should "handle String => (String => String) => String functions" in {
    val mus = new Mustache("{{#bold}}{{name}}{{/bold}}")
    val map = Map(
      "name" -> "world",
      "bold" -> ((str: String, render: String => String) => {
        "<b>" + render(str) + "</b>"
      })
    )
    assert(mus.render(map) === "<b>world</b>")
  }

  it should "correctly remember open and close tags when rendering dynamic templates" in {
    val mus = new Mustache(
      "{{= ** ** =}}**#bold**Hello,**=< >=** <name>!<=__ __=>__/bold__"
    )
    val map =
      Map(
        "name" -> "world",
        "bold" -> ((str: String, render: String => String) => {
          "<b>" + render(" " + str + " ") + "</b>"
        })
      )
    assert(mus.render(map).toString === ("<b> Hello, world! </b>"))
  }

  behavior of "mustache: Nested sections"

  it should "render values from previous context levels" in {
    val temp1 =
      """
      {{#organization}}{{header}}
        id: {{id}}
        name: {{name}}{{/organization}}
      """

    val res1 =
      """
      Hello
        id: 1
        name: org
      """
    val mus = new Mustache(temp1)
    val map = Map(
      "header"       -> "Hello",
      "organization" -> Map("id" -> 1, "name" -> "org")
    )
    assert(mus.render(map) === res1)
  }
