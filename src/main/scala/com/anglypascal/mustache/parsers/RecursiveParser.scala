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

case class TagInfo(otag: String, ctag: String):
  val sreg = raw"(?s)(.*?)($otag.*?$ctag.*)".r
  val treg = raw"(?s)$otag(.*?)$ctag(.*)".r

sealed trait ParserTrait:
  val tag: TagInfo
  def apply(tokens: List[Token])(
      str: String
  ): (List[Token], ParserTrait, String)

final case class StringParser(tag: TagInfo) extends ParserTrait:

  def next: ParserTrait = TagParser(tag)
  def apply(tokens: List[Token])(str: String) =
    str match
      case tag.sreg(a, b) =>
        if a == "" then (tokens, next, b)
        else (StaticTextToken(a) :: tokens, next, b)
      case _ =>
        if str == "" then (tokens, EmptyParser(tag), "")
        else (StaticTextToken(str) :: tokens, EmptyParser(tag), "")

final case class TagParser(tag: TagInfo) extends ParserTrait:
  def apply(tokens: List[Token])(str: String) =
    str match
      case tag.treg(k, b) =>
        val a = k.trim
        if a.startsWith("#") then
          val key = a.substring(1).trim
          (
            IncompleteSection(key, false, tag.otag, tag.ctag) :: tokens,
            StringParser(tag),
            b
          )
        else if a.startsWith("^") then
          val key = a.substring(1).trim
          (
            IncompleteSection(key, true, tag.otag, tag.ctag) :: tokens,
            StringParser(tag),
            b
          )
        else if a.startsWith(">") || a.startsWith("<") then
          val key = a.substring(1).trim
          PartialParser(tag, key)(tokens)(b)
        else if a.startsWith("&") then
          val key = a.substring(1).trim
          (
            UnescapedToken(key, tag.otag, tag.ctag) :: tokens,
            StringParser(tag),
            b
          )
        else if (a.startsWith("{") && a.endsWith("}")) then
          val key = a.substring(1).init.trim
          (
            UnescapedToken(key, tag.otag, tag.ctag) :: tokens,
            StringParser(tag),
            b
          )
        else if (a.startsWith("=") && a.endsWith("=")) then
          val arr  = a.substring(1).init.trim.split(" ")
          val ns   = TokenParser.escape(arr.head.trim)
          val nc   = TokenParser.escape(arr.last.trim)
          val ntag = TagInfo(ns, nc)
          StringParser(ntag)(tokens)(b)
        else if a.startsWith("/") then
          val key = a.substring(1).trim
          @tailrec
          def loop(children: List[Token], t: List[Token]): List[Token] =
            t match
              case Nil => TokenParser.fail(s"closing unopened section tag $key")
              case IncompleteSection(_key, inv, so, sc) :: tail =>
                if key == _key then
                  SectionToken(
                    inv,
                    key,
                    children,
                    so,
                    sc,
                    tag.otag,
                    tag.ctag
                  ) :: tail
                else TokenParser.fail(s"unclosed section tag $key")
              case head :: tail => loop(head :: children, tail)
          val t = loop(List(), tokens)
          StringParser(tag)(t)(b)
        else if a == "" then TokenParser.fail("empty tag")
        else
          (
            EscapedToken(a, tag.otag, tag.ctag) :: tokens,
            StringParser(tag),
            b
          )
      case _ => TokenParser.fail("couldn't match tag complete")

final case class PartialParser(tag: TagInfo, key: String) extends ParserTrait:
  def apply(tokens: List[Token])(str: String) =
    (
      PartialToken(key, tag.otag, tag.ctag) :: tokens,
      StringParser(tag),
      str
    )

final case class EmptyParser(tag: TagInfo) extends ParserTrait:
  def apply(tokens: List[Token])(str: String) = TokenParser.fail("EmptyParser")

class RecursiveParser(val src: Source, val otag: String, val ctag: String):
  def parse(): Token =
    val string = src.mkString
    @tailrec
    def loop(
        tuple: (List[Token], ParserTrait, String)
    ): List[Token] =
      val (tokens, parser, str) = tuple
      if str == "" then tokens
      else loop(parser(tokens)(str))
    val tag    = TagInfo(otag, ctag)
    val tokens = loop(List(), StringParser(tag), string)
    if tokens.length == 1 then tokens(0)
    else RootToken(tokens.reverse)

object TokenParser:
  def escape(str: String): String =
    val esc = Map(
      "*" -> "\\*",
      "_" -> "\\_",
      "{" -> "\\{",
      "}" -> "\\}"
    )
    esc.foldLeft(str)((s, p) => s.replace(p._1, p._2))

  def fail(msg: String) = throw MustacheParseException(0, 0, msg)

@main
def parseTest =
  // for i <- 10000 to 1000000 by 10000 do
  val i     = 100
  val temp1 = "{{haha}}" * i
  val ss    = Source.fromString(temp1)
  val p     = RecursiveParser(ss, raw"\{\{", raw"\}\}")
  val p2    = IterativeParser(ss, "{{", "}}")

  val t1 = System.nanoTime
  val v  = p.parse()
  val t2 = System.nanoTime
  val v2 = p2.parse()
  val t3 = System.nanoTime

  print(i, (t2 - t1) / (t3 - t2))
  println()
