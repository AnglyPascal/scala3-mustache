package com.anglypascal.mustache.tokens
import com.anglypascal.mustache._

case class StaticTextToken(staticText: String) extends Token:
  def render: TokenRender    = (_, _, _) => StringProduct(staticText)
  def templateSource: String = staticText
