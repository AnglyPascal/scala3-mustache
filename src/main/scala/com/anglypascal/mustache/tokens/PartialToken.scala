package com.anglypascal.mustache.tokens

class PartialToken(key: String, otag: String, ctag: String) extends Token:
  def render: TokenRender = (context, partials, callstack) =>
    partials
      .get(key)
      .map(t => t.product(context, partials, t :: callstack))
      .getOrElse(
        throw IllegalArgumentException(s"Partial \"$key\" is not defined")
      )
  def templateSource: String = otag + ">" + key + ctag

  override def toString(): String =
    "PartialToken: " + key
