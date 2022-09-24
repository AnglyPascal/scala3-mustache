package com.anglypascal.mustache.tokens

class StaticTextToken(staticText: String) extends Token:
  def render: TokenRender    = (_, _, _) => StringProduct(staticText)
  def templateSource: String = staticText

  override def toString(): String = 
    "StaticTextToken: " + templateSource
