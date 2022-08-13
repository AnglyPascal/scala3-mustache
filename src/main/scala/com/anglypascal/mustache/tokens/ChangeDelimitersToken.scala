package com.anglypascal.mustache.tokens
import com.anglypascal.mustache._

case class ChangeDelimitersToken(
    newOTag: String,
    newCTag: String,
    otag: String,
    ctag: String
) extends Token:

  def render: TokenRender = (_, _, _) => EmptyProduct
  def templateSource      = otag + "=" + newOTag + " " + newCTag + "=" + ctag
