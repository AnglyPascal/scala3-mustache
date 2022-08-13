package com.anglypascal.mustache.tokens
import com.anglypascal.mustache._

case class IncompleteSection(
    key: String,
    inverted: Boolean,
    otag: String,
    ctag: String
) extends Token:

  private def fail = throw new Exception("""
    Weird thing happend. There is incomplete section in compiled template.
    """)

  def render: TokenRender = fail
  def templateSource: String = fail
