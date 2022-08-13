package com.anglypascal.mustache.tokens
import com.anglypascal.mustache._

case class SectionToken(
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

  private val childrenSource = children.map(_.templateSource).mkString
  private val source =
    startOTag + (if inverted then "^" else "#") + key + startCTag +
      childrenSource +
      endOTag + "/" + key + endCTag

  private val childrenTemplate = new Mustache(
    if children.size == 1 then children(0) else RootToken(children)
  )

  def render: TokenRender = (context, partials, callstack) =>
    def compose = composite(children, context, partials, context :: callstack)
    valueOf(
      key,
      context,
      partials,
      callstack,
      childrenSource,
      renderContent
    ) match
      case null | None =>
        if !inverted then EmptyProduct
        else compose
      case b: Boolean =>
        if !(b ^ inverted) then EmptyProduct
        else compose
      case s: Seq[?] =>
        if inverted then
          if !s.isEmpty then EmptyProduct
          else compose
        else
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
      if template == childrenSource then
        childrenTemplate.render(context, partials, context :: callstack)
      else
        val t = new Mustache(template, startOTag, startCTag)
        t.render(context, partials, context :: callstack)

  def templateSource: String = source
