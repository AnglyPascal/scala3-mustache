package com.anglypascal.mustache.parsers

import com.anglypascal.mustache.tokens.ChangeDelimitersToken
import com.anglypascal.mustache.tokens.EscapedToken
import com.anglypascal.mustache.tokens.IncompleteSection
import com.anglypascal.mustache.tokens.PartialToken
import com.anglypascal.mustache.tokens.RootToken
import com.anglypascal.mustache.tokens.SectionToken
import com.anglypascal.mustache.tokens.StaticTextToken
import com.anglypascal.mustache.tokens.Token
import com.anglypascal.mustache.tokens.UnescapedToken

import scala.annotation.tailrec
import scala.io.Source
import scala.util.matching.Regex

object RecursiveParser:

  private type Matcher[A] = (A, Pos) => (String, String, A, Pos)

  private type State[A] = (String, String, Matcher[A])

  private type Pos = (Int, Int)

  private type Create[A] = String => String => Matcher[A]

  private def fail(row: Int, col: Int, msg: String) =
    throw MustacheParseException(row, col, msg)

  private inline def skipLeft(str: String)(pos: Pos) =
    val s = str.substring(1).trim
    if s == "" then fail(pos._1, pos._2, "Empty Tag")
    else s

  private inline def skipBoth(str: String)(pos: Pos) =
    val s = str.substring(1, str.length - 1).trim
    if s == "" then fail(pos._1, pos._2, "Empty Tag")
    else s

  @tailrec
  private def parser[A](create: Create[A])(
      _a: A,
      tokens: List[Token],
      state: State[A],
      _pos: Pos
  ): Token =
    val (str, tag, a, pos) = state._3(_a, _pos)
    val _tokens =
      if str != null && str != "" then StaticTextToken(str) :: tokens
      else tokens

    if tag == null then
      _tokens foreach {
        _ match
          case IncompleteSection(key, _, _, _) =>
            fail(0, 0, s"Unclosed mustache section \"$key\"")
          case _ => ()
      }
      RootToken(_tokens.reverse)
    else if tag.trim == "" then fail(pos._1, pos._2, "Empty Tag")
    else
      val k = tag.trim
      k(0) match
        case '#' =>
          val key = skipLeft(k)(pos)
          val is  = IncompleteSection(key, false, state._1, state._2)
          parser(create)(a, is :: _tokens, state, _pos)
        case '^' =>
          val key = skipLeft(k)(pos)
          val is  = IncompleteSection(key, true, state._1, state._2)
          parser(create)(a, is :: _tokens, state, _pos)
        case '<' | '>' =>
          val key = skipLeft(k)(pos)
          val pt  = PartialToken(key, state._1, state._2)
          parser(create)(a, pt :: _tokens, state, _pos)
        case '&' =>
          val key = skipLeft(k)(pos)
          val ut  = UnescapedToken(key, state._1, state._2)
          parser(create)(a, ut :: _tokens, state, _pos)
        case '{' if k.charAt(k.length - 1) == '}' =>
          val key = skipBoth(k)(pos)
          val ut  = UnescapedToken(key, state._1, state._2)
          parser(create)(a, ut :: _tokens, state, _pos)
        case '{' if k.charAt(k.length - 1) != '}' =>
          fail(pos._1, pos._2, "unclosed unescaped token brace")
        case '=' =>
          if k.length > 1 && k.charAt(k.length - 1) == '=' then
            val arr = skipBoth(k)(pos).split(raw"\s+")
            if arr.length != 2 then
              fail(pos._1, pos._2, "invalid delimiter tag")
            val o  = arr(0)
            val c  = arr(1)
            val ct = ChangeDelimitersToken(o, c, state._1, state._2)
            parser(create)(a, ct :: _tokens, (o, c, create(o)(c)), _pos)
          else fail(pos._1, pos._2, "invalid delimiter tag")
        case '/' =>
          val key = skipLeft(k)(pos)

          @tailrec
          def loop(c: List[Token], t: List[Token]): List[Token] =
            t match
              case Nil =>
                fail(pos._1, pos._2, s"closing unopened section tag $key")
              case IncompleteSection(_k, inv, so, sc) :: tail =>
                if key == _k then
                  SectionToken(inv, key, c, so, sc, state._1, state._2) :: tail
                else fail(pos._1, pos._2, s"unclosed section tag $key")
              case head :: tail => loop(head :: c, tail)

          parser(create)(a, loop(List(), _tokens), state, _pos)
        case _ =>
          val et = EscapedToken(k, state._1, state._2)
          parser(create)(a, et :: _tokens, state, _pos)

  private val create: Create[Source] =
    otag =>
      ctag =>
        (src, _pos) =>
          enum State:
            case Text, O, Tag, C

          val buf         = StringBuilder()
          var tagPos      = 0
          var state       = State.Text
          var cur: Char   = '\uffff'
          var prev: Char  = '\uffff'
          var curlyBrace  = false
          var str: String = null
          var tag: String = null
          var continue    = true
          var (row, col)  = _pos
          def pos         = (row, col)

          while continue do
            if !src.hasNext then
              continue = false
              state match
                case State.Text =>
                  str = buf.toString
                case State.O =>
                  buf.append(otag.substring(0, tagPos))
                  str = buf.toString
                case State.Tag | State.C =>
                  fail(row, col, "unfinished tag")
            else
              prev = cur
              cur = src.next
              if cur == '\r' || (cur == '\n' && prev != '\r') then
                row += 1
                col = 0
              else col += 1
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
                      continue = false
                  else buf.append(cur)
                case State.C =>
                  if cur == ctag(tagPos) then
                    if tagPos == ctag.length - 1 then
                      tag = buf.toString
                      buf.clear()
                      continue = false
                    else tagPos += 1
                  else
                    buf.append(ctag.substring(0, tagPos))
                    buf.append(cur)
                    state = State.Tag

          (str, tag, src, pos)

  def parse(src: Source, otag: String, ctag: String) =
    parser(create)(src, List(), (otag, ctag, create(otag)(ctag)), (1, 0))

  @main
  def recparseTest =
    def loop() =
      val n = 10000
      for j <- 1 to 10 do
        val i = j * n
        val temp1 =
          "{{#haha}}{{a}}{{/haha}}{{=_ _=}}_hello__^hi_bruh_/hi__={{ }}=_" * i
        val temp2 = "{{#h}}{{b}}{{/h}}"
        val ss    = Source.fromString(temp2)

        val t2 = System.nanoTime
        val v2 = IterativeParser(ss, "{{", "}}").parse()
        val t3 = System.nanoTime
        val v3 = RecursiveParser.parse(ss, "{{", "}}")
        val t4 = System.nanoTime

        println((t3 - t2).toDouble / (t4 - t3).toDouble)

    // val m = create("{{")("}}")
    // val s = Source.fromString("Hello, {{#person}}{{name}}!")
    // val t = parse(s, "{{", "}}")
    // println(t)

    loop()
