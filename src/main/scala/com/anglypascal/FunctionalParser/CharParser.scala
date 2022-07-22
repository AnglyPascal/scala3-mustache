package com.anglypascal.FunctionalParser

object CharParser:

  import Parser._

  def item: Parser[Char] = str =>
    if str.length == 0 then List()
    else List((str.head, str.tail))

  def sat(pred: Char => Boolean): Parser[Char] = str =>
    item(str).filter(p => pred(p._1))

  val char: Char => Parser[Char] = ch => sat(_ == ch)

  val unchar: Char => Parser[Char] = ch => sat(_ != ch)

  val alphabets: Parser[String] = 
    val alph: Parser[Char] = 
      ('a' to 'z').map(char).fold(char('.'))(_<||>_)
    some(alph).map(_.mkString)

  def string: String => Parser[String] = str =>
    str match
      case "" => remit("")
      case s =>
        val done = (c: Char) => (cs: String) => remit(c +: cs)
        val tail = (c: Char) => string(s.tail) >>= done(c)
        char(s.head) >>= tail

  def andThen(a: Parser[Char])(b: Parser[Char]): Parser[String] =
    val f = (c: Char) => (s: String) => b(s).map(p => (p._1.toString + c, p._2))
    a >>= f

  extension (pc: Parser[Char])
    def +:(ps: Parser[String]): Parser[String] = 
      val f = (c: Char) =>
        (str: String) => ps(str).map(p => (c +: p._1, p._2))
      pc >>= f

  extension (ps: Parser[String])
    def +(pc: Parser[Char]): Parser[String] = 
      val f = (s: String) =>
        (str: String) => pc(str).map(p => (s + p._1, p._2))
      ps >>= f

    def ++:(ps2: Parser[String]): Parser[String] = 
      val f = (s: String) =>
        (str: String) => ps2(str).map(p => (s + p._1, p._2))
      ps >>= f
