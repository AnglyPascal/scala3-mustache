package com.anglypascal.mustache.tokens
import com.anglypascal.mustache._

class StaticTextToken(staticText: String) extends Token:
  def render: TokenRender    = (_, _, _) => StringProduct(staticText)
  def templateSource: String = staticText

  override def toString(): String = 
    "StaticTextToken: " + templateSource
