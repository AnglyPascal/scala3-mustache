package com.anglypascal.mustache.tokens
import com.anglypascal.mustache._

class RootToken(children: List[Token]) extends Token with CompositeToken:
  private val childrenSource = children.map(_.templateSource).mkString

  def render: TokenRender    = composite(children, _, _, _)
  def templateSource: String = childrenSource

  override def toString(): String = 
    "RootToken:\n" + children.mkString("\n")
