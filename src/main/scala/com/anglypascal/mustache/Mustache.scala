package com.anglypascal.mustache

import com.anglypascal.mustache.tokens.{Token, TokenProduct}
import com.anglypascal.mustache.parsers.IterativeParser
import scala.io.Source
import com.anglypascal.mustache.parsers.RecursiveParser

class Mustache(root: Token) extends MustacheHelperSupport:

  def this(source: Source, open: String = "{{", close: String = "}}") =
    // this((IterativeParser(source, open, close)).parse())
    this(RecursiveParser.parse(source, open, close))

  def this(str: String, open: String, close: String) =
    this(Source.fromString(str), open, close)

  def this(str: String) = this(Source.fromString(str))

  private val compiledTemplate: Token = root

  def render(
      context: Any = null,
      partials: Map[String, Mustache] = Map(),
      callstack: List[Any] = List(this)
  ): String =
    product(context, partials, callstack).toString

  def product(
      context: Any = null,
      partials: Map[String, Mustache] = Map(),
      callstack: List[Any] = List(this)
  ): TokenProduct =
    compiledTemplate.render(context, partials, callstack)
