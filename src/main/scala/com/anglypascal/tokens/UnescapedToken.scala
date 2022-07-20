package com.anglypascal.mustache.tokens
import com.anglypascal.mustache._

case class UnescapedToken(key: String, otag: String, ctag: String)
    extends Token
    with ContextHandler
    with ValuesFormatter:

  private val source = otag + "&" + key + ctag

  def render: TokenRender = (context, partials, callstack) =>
    val v = format(
      valueOf(key, context, partials, callstack, "", defaultRender(otag, ctag))
    )
    new TokenProduct:
      val maxLength                       = v.length
      def write(out: StringBuilder): Unit = out.append(v)

  def templateSource: String = source
