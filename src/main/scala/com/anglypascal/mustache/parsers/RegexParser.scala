package com.anglypascal.mustache.parsers

import scala.util.matching.Regex

object RegexParser:
  type Parser[A] = String => (Option[A], String)

  def join[A](f: (A, A) => A)(ps: Parser[A]*): Parser[A] = src =>
    ps.length match
      case 0 => (None, src)
      case 1 => ps(0)(src)
      case _ =>
        val (opt, str) = ps(0)(src)
        opt match
          case None => (None, str)
          case Some(s) =>
            val (nopt, nstr) = join(f)(ps.tail: _*)(str)
            nopt match
              case None     => (None, src)
              case Some(s2) => (Some(f(s, s2)), nstr)

  def joinStr(ps: Parser[String]*) = join[String](_ + _)(ps: _*)

  def map[A, B](ps: Parser[A])(f: A => B): Parser[B] = src =>
    val (o, s) = ps(src)
    (o.map(f), s)

  inline def regParser(reg: Regex): Parser[String] = src =>
    src match
      case reg(a, b) => (Some(a), b)
      case _         => (None, src)

  def strParser(str: String): Parser[String] =
    regParser(raw"($str)(.*)".r)

  def tagParser(otag: String, key: String, ctag: String): Parser[String] =
    map(regParser(raw"$otag\s*($key)\s*$ctag(.*)".r))(_.trim)

  def anyTagParser(otag: String, ctag: String): Parser[String] =
    tagParser(otag, ".*?", ctag)

  def many[A, B](p: Parser[A])(default: B)(f: (B, A) => B): Parser[B] = src =>
    val (o, s) = p(src)
    o match
      case None    => (Some(default), src)
      case Some(r) => many(p)(f(default, r))(f)(s)

  def some[A, B](p: Parser[A])(default: B)(f: (B, A) => B): Parser[B] = src =>
    val (o, s) = p(src)
    o match
      case None    => (None, src)
      case Some(r) => many(p)(f(default, r))(f)(s)

  def orElse[A](p1: Parser[A], p2: Parser[A]): Parser[A] = src =>
    val (o, s) = p1(src)
    o match
      case None => p2(src)
      case some => (o, s)

  def untilGreedy(str: String): Parser[String] =
    regParser(raw"(.*)($str.*)".r)

  def untilLazy(str: String): Parser[String] =
    regParser(raw"(.*?)($str.*)".r)

  def untilLazyConsuming(str: String): Parser[String] =
    regParser(raw"(.*?)$str(.*)".r)

  def sectionParser(otag: String, ctag: String): Parser[String] = src =>
    val (o, s) = anyTagParser(otag + raw"\s*#\s*", ctag)(src)
    o match
      case None => (o, s)
      case Some(key) =>
        val str = otag + raw"\s*/\s*$key\s*" + ctag
        println(str)
        untilLazyConsuming(str)(s)

  // @main
  def regParser =
    val src = "{{ hello}}haha"
    val t   = anyTagParser(raw"\{\{", raw"\}\}")
    val s   = strParser("haha")
    println(joinStr(t, s)(src))

    val m = many(s)("")(_ + _)
    println(m("hahahahahahahahabra"))

    val u  = untilGreedy("bruh")
    val su = "hello bruh bye bruh"
    println(u(su))

    val sec  = sectionParser(raw"\{\{", raw"\}\}")
    val ssec = "{{# hello }} omg {{ /hello}}"
    println(sec(ssec))
