package com.anglypascal.FunctionalParser

object CharParser:

  import Parser._

  def item: Parser[Char] = str =>
    if str.length == 0 then List()
    else List((str.head, str.tail))

  def sat(pred: Char => Boolean): Parser[Char] = str =>
    item(str).filter(p => pred(p._1))

  def char(ch: Char): Parser[Char] = sat(_ == ch)

  def unchar(ch: Char): Parser[Char] = sat(_ != ch)


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

  def prependChar(first: Parser[Char])(second: Parser[String]): Parser[String] =
    val f = (c: Char) =>
      (str: String) => second(str).map(p => (c +: p._1, p._2))
    first >>= f

  def appendChar(first: Parser[String])(second: Parser[Char]): Parser[String] =
    val f = (s: String) =>
      (str: String) => second(str).map(p => (s + p._1, p._2))
    first >>= f

  // makes a parser that matches
  // a b c b c b c b c b
  // pushes the result into a stack
  // need to mimic many/some
  def combine(
      a: Parser[String],
      b: Parser[String],
      c: Parser[String]
  ): Parser[List[String]] = ???
