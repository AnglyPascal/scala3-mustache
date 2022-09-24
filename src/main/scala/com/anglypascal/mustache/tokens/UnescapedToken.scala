package com.anglypascal.mustache.tokens

import com.anglypascal.mustache.ContextHandler
import com.anglypascal.mustache.Mustache
import com.anglypascal.mustache.ValuesFormatter

case class UnescapedToken(key: String, otag: String, ctag: String)
    extends Token
    with ContextHandler:

  import ValuesFormatter.format

  private inline def defaultRender(otag: String, ctag: String): Renderer =
    (context, partials, callstack) =>
      str => {
        val t = new Mustache(str, otag, ctag)
        t.render(context, partials, callstack)
      }

  private val source = otag + "&" + key + ctag

  def render: TokenRender = (context, partials, callstack) =>
    val v = format(
      valueOf(key, context, partials, callstack, "", defaultRender(otag, ctag))
    )
    new TokenProduct:
      val maxLength                       = v.length
      def write(out: StringBuilder): Unit = out.append(v)

  def templateSource: String = source

  override def toString(): String = 
    "UnescapedToken: " + templateSource
