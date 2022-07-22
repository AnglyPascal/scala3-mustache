package com.anglypascal.FunctionalParser

class StringParser(otag: String, ctag: String):
  import CharParser.*
  import Parser.*

  /** need to figure out the algebra here what if we keep pushing things inside
    * a stack instead of a queue?
    */
  val uc    = sat(c => c != otag.head)
  val tc    = sat(c => c != ctag.head)
  val text  = many(uc).map(_.mkString)
  val text1 = prependChar(char(otag.head))(text)
  val beg   = string(otag) <||> text1
  val tag   = many(tc).map(_.mkString)
  val tag1  = appendChar(tag)(char(ctag.head))
  val tag2  = appendChar(tag)(char(ctag.head))
  val end   = string(ctag) <||> tag2

  // val dumb = text cons (beg cons (tag cons (end cons (text cons nil[String]))))
  val tillOtag = chain(text, beg, text1)
  val tillCtag = chain(tag, end, tag1)
  val dumb = tillOtag //cons (tillCtag.lift)

  def test(str: String) = println(dumb(str))

@main
def main: Unit =
  import CharParser.*
  import Parser.*

  val p        = new StringParser("&!", "}}")
  val template = "test string& with lots of & and & one &! after &! then &"
  p.test(template)
