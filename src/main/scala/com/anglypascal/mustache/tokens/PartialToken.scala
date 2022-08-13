package com.anglypascal.mustache.tokens
import com.anglypascal.mustache._

case class PartialToken(key: String, otag: String, ctag: String) extends Token:
  def render: TokenRender = (context, partials, callstack) =>
    partials
      .get(key)
      .map((t: Mustache) => t.product(context, partials, t :: callstack))
      .getOrElse(
        throw new java.lang.IllegalArgumentException(
          "Partial \"" + key + "\" is not defined"
        )
      )
  def templateSource: String = otag + ">" + key + ctag
