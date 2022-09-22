package com.anglypascal.mustache.tokens
import com.anglypascal.mustache._

class PartialToken(key: String, otag: String, ctag: String) extends Token:
  def render: TokenRender = (context, partials, callstack) =>
    partials
      .get(key)
      .map((t: Mustache) => t.product(context, partials, t :: callstack))
      .getOrElse(
        throw new IllegalArgumentException(s"Partial \"$key\" is not defined")
      )
  def templateSource: String = otag + ">" + key + ctag

  override def toString(): String = 
    "PartialToken: " + key
