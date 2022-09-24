package com.anglypascal.mustache.tokens

import com.anglypascal.mustache.ContextHandler
import com.anglypascal.mustache.Mustache

class SectionToken(
    inverted: Boolean,
    key: String,
    children: List[Token],
    startOTag: String,
    startCTag: String,
    endOTag: String,
    endCTag: String
) extends Token
    with ContextHandler
    with CompositeToken:

  private lazy val childrenSource = children.map(_.templateSource).mkString

  def render: TokenRender = (context, partials, callstack) =>
    def compose = composite(children, context, partials, context :: callstack)
    val v = valueOf(
      key,
      context,
      partials,
      callstack,
      childrenSource,
      renderContent
    )

    v match
      case null | None =>
        if !inverted then EmptyProduct
        else compose
      case b: Boolean =>
        if !(b ^ inverted) then EmptyProduct
        else compose
      case s: Seq[?] if inverted =>
        if !s.isEmpty then EmptyProduct
        else compose
      case s: Seq[?] if !inverted =>
        val tasks = for
          element <- s
          token   <- children
        yield (token, element)
        composite(tasks, partials, context :: callstack)
      case str: String =>
        if inverted then EmptyProduct
        else StringProduct(str)
      case other =>
        if inverted then EmptyProduct
        else composite(children, other, partials, context :: callstack)

  private def renderContent: Renderer = (context, partials, callstack) =>
    template =>
      val mus =
        if template == childrenSource then
          new Mustache(
            if children.size == 1 then children(0)
            else RootToken(children)
          )
        else new Mustache(template, startOTag, startCTag)
      mus.render(context, partials, context :: callstack)

  private lazy val source =
    startOTag + (if inverted then "^" else "#") + key + startCTag +
      childrenSource +
      endOTag + "/" + key + endCTag

  def templateSource: String = source

  override def toString(): String = 
    s"SectionToken: $key, [" + children.mkString(", ") + "]"
