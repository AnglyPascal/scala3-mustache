package com.anglypascal.mustache.tokens

import com.anglypascal.mustache.TypeAliases

trait Token extends TypeAliases:
  def render: TokenRender
  def templateSource: String

trait CompositeToken:
  this: Token =>

  protected def composite(
      tokens: List[Token],
      context: Any,
      partials: Partials,
      callstack: CallStack
  ): TokenProduct =
    composite(tokens.map((_, context)), partials, callstack)

  protected def composite(
      tasks: Seq[(Token, Any)],
      partials: Partials,
      callstack: CallStack
  ): TokenProduct =
    val result = tasks.map((t, c) => t.render(c, partials, callstack))
    val len    = result.foldLeft(0)(_ + _.maxLength)

    new TokenProduct:
      val maxLength: Int = len
      def write(out: StringBuilder) =
        result.map(_.write(out))
