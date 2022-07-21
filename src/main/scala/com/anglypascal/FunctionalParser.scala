// import scala.io.Source
// import scala.annotation.tailrec

object FunctionalParser:
  private type Parser[A] = String => List[(A, String)]

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

  def item: Parser[Char] = str =>
    if str.length == 0 then List()
    else List((str.head, str.tail))

  def sat(pred: Char => Boolean): Parser[Char] = str =>
    item(str).filter(p => pred(p._1))

  def char(ch: Char): Parser[Char] = sat(_ == ch)

  def unchar(ch: Char): Parser[Char] = sat(_ != ch)

  def fail[A]: Parser[A] = _ => List(): List[(A, String)]

  def remit[A](a: A): Parser[A] = str => List((a, str))

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

  // apply parser at least once or as many times possible
  def many[A](parser: Parser[A]): Parser[List[A]] =
    some(parser) <||> remit(List())

  // apply parser zero or more times
  def some[A](parser: Parser[A]): Parser[List[A]] =
    val done = (a: A) => (as: List[A]) => remit(a :: as)
    val ps   = (a: A) => many(parser) >>= done(a)
    parser >>= ps

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
  def combine(a: Parser[String], b: Parser[String], c: Parser[String]): Parser[List[String]] = ???

class Parser(otag: String, ctag: String):
  import FunctionalParser.*

  /** need to figure out the algebra here
   *  what if we keep pushing things inside a stack instead of a queue?
   */
  val uc    = sat(c => c != otag.head)
  val tc    = sat(c => c != ctag.head)
  val text  = many(uc).map(_.mkString)
  val text1 = prependChar(char(otag.head))(text)
  val beg   = string(otag) <||> text1
  val tag   = many(tc).map(_.mkString)
  val tag1   = appendChar(tag)(char(ctag.head))
  val tag2   = appendChar(tag)(char(ctag.head))
  val end   = string(ctag) <||> tag2

  // val dumb = text cons (beg cons (tag cons (end cons (text cons nil[String]))))
  val dumb = (text cons many(beg)) >> string(otag)


  def test(str: String) = println(dumb(str))

@main
def main: Unit =
  import FunctionalParser.*

  val p        = new Parser("{{", "}}")
  val template = "hello { ha {{{name}}} something"
  p.test(template)
