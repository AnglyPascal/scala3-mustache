package com.anglypascal.FunctionalParser

class StringParser(otag: String, ctag: String):
  import CharParser.*
  import Parser.*

  /** need to figure out the algebra here what if we keep pushing things inside
    * a stack instead of a queue?
    */
  val tc    = sat(c => c != ctag.head)
  val tag   = many(tc).map(_.mkString)
  val tag1  = char(ctag.head) +: tag
  val curlt = char('{') +: tag ++: char('}') +: tag
  val end   = string(ctag)

  val tillCtagU = (str: String) =>
    if str.length > 0 && str.head == '{' then chain(curlt, end, tag1)(str)
    else chain(tag, end, tag1)(str)

  val tillCtag = tillCtagU

  val oc         = sat(c => c != otag.head)
  val text       = many(oc).map(_.mkString)
  val cappedText = char(otag.head) +: text
  val beg        = string(otag)
  val tillOtag   = chain(text, beg, cappedText)

  val dumb = tillOtag//altSome(tillOtag, tillCtag)

  def test(str: String) = dumb(str).map(_._1)

@main
def main: Unit =
  import CharParser.*
  import Parser.*

  val p        = new StringParser("{{", "}}")
  val template = """
<h1>{{header}}</h1>
{{#bug}}
{{/bug}}

{{#items}}
  {{#first}}
    <li><strong>{{name}}</strong></li>
  {{/first}}
  {{#link}}
    <li><a href="{{url}}">{{name}}</a></li>
  {{/link}}
{{/items}}

{{#empty}}
  <p>The list is empty.</p>
{{/empty}}
  """ * 1000

  def tt = "a" * 10000
  val t = alphabets(tt)
  println("done")

  // val t = "cucuc"
  // p.test(t)
