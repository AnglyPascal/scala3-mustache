package com.anglypascal.mustache.tokens
import com.anglypascal.mustache._

case class IncompleteSection(
    key: String,
    inverted: Boolean,
    otag: String,
    ctag: String
) extends Token:

  private def fail =
    throw new Exception(
      s"There is incomplete section in compiled template with key $key"
    )

  def render: TokenRender    = fail
  def templateSource: String = fail

  override def toString(): String =
    (if inverted then "inverted"
     else "") + "IncompleteSection: " + key
