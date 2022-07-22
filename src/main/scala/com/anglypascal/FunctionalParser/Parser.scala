package com.anglypascal.FunctionalParser

object Parser:
  type Parser[A] = String => List[(A, String)]

  extension [A](parser: Parser[A])
    def filter(pred: A => Boolean): Parser[A] = str =>
      parser(str).filter(p => pred(p._1))

    def map[B](f: A => B): Parser[B] = str =>
      parser(str).map(p => (f(p._1), p._2))

    def >>[B](next: Parser[B]): Parser[B] = str =>
      parser(str).flatMap(p => next(p._2))

    def >>=[B](f: A => Parser[B]): Parser[B] = str =>
      parser(str).flatMap(p => f(p._1)(p._2))

    def <|>(second: Parser[A]): Parser[A] = str =>
      parser(str).concat(second(str))

    def <||>(that: Parser[A]): Parser[A] = str =>
      val p = parser(str)
      if p.length == 0 then that(str)
      else p

    def lift: Parser[List[A]] = str => parser(str).map(p => (List(p._1), p._2))

    def cons(second: Parser[List[A]]): Parser[List[A]] =
      val f = (as: A) =>
        (str: String) => second(str).map(p => (as :: p._1, p._2))
      parser >>= f


  def nil[A]: Parser[List[A]] = str => List((List(), str))

  def fail[A]: Parser[A] = _ => List(): List[(A, String)]

  def remit[A](a: A): Parser[A] = str => List((a, str))

  // apply parser zero or more times
  def many[A](parser: Parser[A]): Parser[List[A]] =
    some(parser) <||> remit(List())

  // apply parser one or more times
  def some[A](parser: Parser[A]): Parser[List[A]] =
    val done = (a: A) => (as: List[A]) => remit(a :: as)
    val ps   = (a: A) => many(parser) >>= done(a)
    parser >>= ps

  def altMany[A](a: Parser[A], b: Parser[A]): Parser[List[A]] =
    altSome(a, b) <||> remit(List())

  def altSome[A](a: Parser[A], b: Parser[A]): Parser[List[A]] =
    val done = (x: A) => (xs: List[A]) => remit(x :: xs)
    val parser = a <||> b
    val ps = (x: A) => altMany(a, b) >>= done(x)
    parser >>= ps

  def chain[A](a: Parser[A], b: Parser[A], c: Parser[A]): Parser[List[A]] = 
    val f = (x: A) => (s: String) => altMany(b, c)(s).map(p => (x:: p._1, p._2))
    a >>= f
    
