package com.anglypascal.mustache.parsers

import scala.annotation.tailrec
import com.anglypascal.mustache.tokens.StaticTextToken
import com.anglypascal.mustache.tokens.Token
import com.anglypascal.mustache.tokens.EscapedToken
import com.anglypascal.mustache.tokens.UnescapedToken
import com.anglypascal.mustache.tokens.RootToken
import com.anglypascal.mustache.tokens.IncompleteSection
import com.anglypascal.mustache.tokens.SectionToken
import com.anglypascal.mustache.tokens.PartialToken
import scala.io.Source
import scala.util.matching.Regex

object RecursiveParser:

  type Matcher[A] = A => (String, String, A)

  type State[A] = (String, String, Matcher[A])

  type Create[A] = String => String => Matcher[A]

  def fail(msg: String) = throw MustacheParseException(0, 0, msg)

  @tailrec
  def parser[A](create: Create[A])(
      _a: A,
      tokens: List[Token],
      state: State[A]
  ): Token =
    val (str, tag, a) = state._3(_a)
    val st            = StaticTextToken(str)
    if tag == null then RootToken((st :: tokens).reverse)
    else if tag == "" then fail("Empty Tag")
    else
      val k = tag.trim
      k(0) match
        case '#' =>
          val key = k.substring(1).trim
          val is  = IncompleteSection(key, false, state._1, state._2)
          parser(create)(a, is ::st ::  tokens, state)
        case '^' =>
          val key = k.substring(1).trim
          val is  = IncompleteSection(key, true, state._1, state._2)
          parser(create)(a, is :: st :: tokens, state)
        case '<' | '>' =>
          val key = k.substring(1).trim
          val pt  = PartialToken(key, state._1, state._2)
          parser(create)(a, pt :: st :: tokens, state)
        case '&' =>
          val key = k.substring(1).trim
          val ut  = UnescapedToken(key, state._1, state._2)
          parser(create)(a, ut :: st :: tokens, state)
        case '{' if k.charAt(k.length - 1) == '}' =>
          val key = k.substring(1, k.length - 1).trim
          val ut  = UnescapedToken(key, state._1, state._2)
          parser(create)(a, ut :: st :: tokens, state)
        case '{' if k.charAt(k.length - 1) != '}' =>
          fail("unclosed unescaped token brace")
        case '=' if k.charAt(k.length - 1) == '=' =>
          val arr = k.substring(1, k.length - 1).trim.split(" ")
          val o   = arr(0)
          val c   = arr(1)
          parser(create)(a, st :: tokens, (state._1, state._2, create(o)(c)))
        case '/' =>
          val key = k.substring(1).trim

          @tailrec
          def loop(c: List[Token], t: List[Token]): List[Token] =
            t match
              case Nil => fail(s"closing unopened section tag $key")
              case IncompleteSection(_k, inv, so, sc) :: tail =>
                if key == _k then
                  SectionToken(inv, key, c, so, sc, state._1, state._2) :: tail
                else TokenParser.fail(s"unclosed section tag $key")
              case head :: tail => loop(head :: c, tail)

          parser(create)(a, loop(List(), st :: tokens), state)
        case _ =>
          val et = EscapedToken(k, state._1, state._2)
          parser(create)(a, et :: st :: tokens, state)

  def escape(str: String): String =
    val esc = Map(
      "*" -> "\\*",
      "_" -> "\\_",
      "{" -> "\\{",
      "}" -> "\\}"
    )
    esc.foldLeft(str)((s, p) => s.replace(p._1, p._2))

  val create: Create[String] =
    otag =>
      ctag =>
        val o   = escape(otag)
        val c   = escape(ctag)
        val reg = raw"(?s)(.*?)$o(.*?)$c(.*)".r
        str =>
          str match
            case reg(a, b, c) => (a, b, c)
            case _            => (str, null, "")

  def parse(str: String, otag: String, ctag: String) =
    val matcher = create(otag)(ctag)
    parser(create)(str, List(), (otag, ctag, matcher))

  // @main
  def recparseTest =
    val s = "haha{{hello}}"
    val v = parse(s, "{{", "}}")
    println(v)
