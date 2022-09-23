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
import scala.collection.mutable.StringBuilder.apply

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
          parser(create)(a, is :: st :: tokens, state)
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

  val createStr: Create[String] =
    otag =>
      ctag =>
        val o   = escape(otag)
        val c   = escape(ctag)
        val reg = raw"(?s)(.*?)$o(.*?)$c(.*)".r
        str =>
          str match
            case reg(a, b, c) => (a, b, c)
            case _            => (str, null, "")

  val create: Create[Source] =
    otag =>
      ctag =>
        src =>
          enum State:
            case Text, O, Tag, C

          val buf            = StringBuilder()
          var tagPos         = 0
          var state          = State.Text
          var cur: Char      = '\uffff'
          var prev: Char     = '\uffff'
          var curlyBrace     = false
          var str            = ""
          var tag: String    = null
          var shouldContinue = true

          while shouldContinue do
            if !src.hasNext then
              shouldContinue = false
              state match
                case State.Text | State.O =>
                  str = buf.toString
                case State.Tag | State.C =>
                  fail("unfinished tag")
            else
              cur = src.next
              state match
                case State.Text =>
                  if cur == otag(0) then
                    if otag.length > 1 then
                      tagPos = 1
                      state = State.O
                    else
                      str = buf.toString
                      buf.clear()
                      state = State.Tag
                  else buf.append(cur)
                case State.O =>
                  if cur == otag(tagPos) then
                    if tagPos == otag.length - 1 then
                      str = buf.toString
                      buf.clear()
                      state = State.Tag
                    else tagPos += 1
                  else
                    buf.append(otag.substring(0, tagPos))
                    buf.append(cur)
                    state = State.Text
                case State.Tag =>
                  if buf.isEmpty && cur == '{' then
                    curlyBrace = true
                    buf.append(cur)
                  else if curlyBrace && cur == '}' then
                    curlyBrace = false
                    buf.append(cur)
                  else if cur == ctag(0) then
                    if ctag.length > 1 then
                      tagPos = 1
                      state = State.C
                    else
                      tag = buf.toString
                      buf.clear()
                      shouldContinue = false
                  else buf.append(cur)
                case State.C =>
                  if cur == ctag(tagPos) then
                    if tagPos == ctag.length - 1 then
                      tag = buf.toString
                      buf.clear()
                      shouldContinue = false
                    else tagPos += 1
                  else
                    buf.append(ctag.substring(0, tagPos))
                    buf.append(cur)
                    state = State.Tag

          (str, tag, src)

  def parseStr(str: String, otag: String, ctag: String) =
    parser(createStr)(str, List(), (otag, ctag, createStr(otag)(ctag)))

  def parse(src: Source, otag: String, ctag: String) =
    parser(create)(src, List(), (otag, ctag, create(otag)(ctag)))

  @main
  def recparseTest =
    // val s = "haha{{#hello}}{{bruh}}{{/hello}}"
    // val v = parse(s, "{{", "}}")
    // println(v)
    val m = create("{{")("}}")
    val s = Source.fromString(
      "{{#haha}}{{a}}{{/haha}}{{=_ _=}}_hello__^hi_bruh_/hi__={{ }}=_"
    )
    val t = parse(s, "{{", "}}")
    println(t)
