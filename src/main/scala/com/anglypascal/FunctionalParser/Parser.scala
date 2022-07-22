package com.anglypascal.FunctionalParser

import scala.annotation.tailrec

def consPair[A, B](x: A)(p: (List[A], B)): (List[A], B) = (x :: p._1, p._2)

object Parser:
  type Parser[A] = String => List[(A, String)]

  extension [A](parser: Parser[A])
    def filter(pred: A => Boolean): Parser[A] = str =>
      parser(str).filter(p => pred(p._1))

    def map[B](f: A => B): Parser[B] = str =>
      parser(str).map(p => (f(p._1), p._2))

    def ?>[B](f: A => B): Parser[B] = str =>
      parser(str).map(p => (f(p._1), p._2))

    def >>:[B](next: Parser[B]): Parser[B] = str =>
      parser(str).flatMap(p => next(p._2))

    def >>=[B](f: A => Parser[B]): Parser[B] = str =>
      parser(str).flatMap(p => f(p._1)(p._2))

    def <|>(second: Parser[A]): Parser[A] = str =>
      parser(str).concat(second(str))

    def <||>(that: Parser[A]): Parser[A] = str =>
      val p = parser(str)
      if p.length == 0 then that(str)
      else p

    def trim: Parser[A] = str => parser(str).take(1)

    def lift: Parser[List[A]] = str => parser(str).map(p => (List(p._1), p._2))

    def cons(second: Parser[List[A]]): Parser[List[A]] =
      val f = (as: A) => (str: String) => second(str).map(consPair(as))
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

  def tryOrMany[A](a: Parser[A], b: Parser[A]): Parser[List[A]] =
    tryOrSome(a, b) <||> remit(List())

  /** this is running (a or b) many times. but we need to do (if a then a else
    * b) many times
    */
  def tryOrSome[A](a: Parser[A], b: Parser[A]): Parser[List[A]] =
    val done   = (x: A) => (xs: List[A]) => remit(x :: xs)
    val parser = a <||> b
    val ps     = (x: A) => tryOrMany(a, b) >>= done(x)
    a.lift <||> (b >>= ps)

  def chain[A](init: Parser[A], a: Parser[A], b: Parser[A]): Parser[List[A]] =
    val f = (x: A) => (s: String) => tryOrSome(a, b)(s).map(consPair(x))
    init >>= f

  // def altMany[A](a: Parser[A], b: Parser[A]): Parser[List[A]] =
  //   altSome(a, b) <||> remit(List())

  // def altSome[A](a: Parser[A], b: Parser[A]): Parser[List[A]] =
  //   // val f = (x: A) => (str: String) => b.trim(str).map(p => (List(x, p._1), p._2))
  //   // val parser = a.trim >>= f
  //   val d1 = (x: A) => (xs: List[A]) => remit(x :: xs)
  //   val p1 = (x: A) => altMany(a, b) >>= d1(x)
  //   val bb = b >>= p1
  //   val d2 = (x: A) => (xs: List[A]) => remit(x :: xs)
  //   val p2 = (x: A) => bb >>= d1(x)
  //   a >>= p2

  def alternate[A](a: Parser[A], b: Parser[A]): Parser[List[A]] = str =>
    // @tailrec
    def loop[A](results: List[A], a: Parser[A], b: Parser[A]): Parser[List[A]] =
      s0 =>
        val xs = a(s0)
        if xs.length == 0 then List((results, s0))
        else
          val (ls, s1) = xs.head
          val ys       = b(s1)
          if ys.length == 0 then List(): List[(List[A], String)]
          else
            val (rs, s2) = ys.head
            loop(rs :: (ls :: results), a, b)(s2)

    loop(List(), a, b)(str).map(p => (p._1.reverse, p._2))


/** issues:
 *  1. No way to return error
 *  2. VERY inefficient. The recursive calls are not lazy maybe? The many function
 *     seems the culprit. Need to write it with tail recursion. Also alternate.
 */
