package com.anglypascal.mustache
import com.anglypascal.mustache.tokens._

import scala.io.Source
import scala.annotation.tailrec

case class MustacheParseException(line: Int, msg: String)
    extends Exception("Line " + line + ": " + msg)

private class ParserState
private object Text extends ParserState
private object OTag extends ParserState
private object Tag  extends ParserState
private object CTag extends ParserState

private class Parser(val src: Source, var otag: String, var ctag: String)
    extends ParserTrait

private trait ParserTrait:
  val src: Source
  var otag: String
  var ctag: String

  var state: ParserState     = Text
  var tagPosition: Int       = 0
  var line: Int              = 1
  var prev: Char             = '\uffff'
  var cur: Char              = '\uffff'
  var curlyBraceTag: Boolean = false
  var stack: List[Token]     = List()

  val buf = new StringBuilder(8192)

  def parse(): Token =
    while consume do
      state match
        case Text =>
          if cur == otag.charAt(0) then
            if otag.length > 1 then
              tagPosition = 1
              state = OTag
            else
              staticText()
              state = Tag
          else buf.append(cur)

        case OTag =>
          if cur == otag.charAt(tagPosition) then
            if tagPosition == otag.length - 1 then
              staticText()
              state = Tag
            else tagPosition += 1
          else
            notOTag()
            buf.append(cur)

        case Tag =>
          if buf.isEmpty && cur == '{' then
            curlyBraceTag = true
            buf.append(cur)
          else if curlyBraceTag && cur == '}' then
            curlyBraceTag = false
            buf.append(cur)
          else if cur == ctag.charAt(0) then
            if ctag.length > 1 then
              tagPosition = 1
              state = CTag
            else tag()
          else buf.append(cur)

        case CTag =>
          if cur == ctag.charAt(tagPosition) then
            if tagPosition == ctag.length - 1 then tag()
            else tagPosition += 1
          else
            notCTag()
            buf.append(cur)

    state match
      case Text => staticText()
      case OTag =>
        notOTag()
        staticText()
      case Tag => fail("Unclosed tag \"" + buf.toString + "\"")
      case CTag =>
        notCTag()
        staticText()

    stack.foreach {
      case IncompleteSection(key, _, _, _) =>
        fail("Unclosed mustache section\"" + key + "\"")
      case _ =>
    }

    val result = stack.reverse
    if result.size == 1 then result(0)
    else RootToken(result)

  private def fail[A](msg: String): A = throw MustacheParseException(line, msg)

  private def consume: Boolean =
    prev = cur
    if src.hasNext then
      cur = src.next()
      if cur == '\r' || (cur == '\n' && prev != '\r') then line += 1
      true
    else false

  private def notOTag(): Unit =
    buf.append(otag.substring(0, tagPosition))
    state = Text

  private def notCTag(): Unit =
    buf.append(otag.substring(0, tagPosition))
    state = Text

  private def reduce: String =
    var r = buf.toString
    buf.clear()
    r

  private def staticText(): Unit =
    val r = reduce
    if r.length > 0 then stack = StaticTextToken(r) :: stack

  private def checkContent(content: String): String =
    val trimmed = content.trim
    if trimmed.length == 0 then fail("Empty tag")
    else trimmed

  def tag(): Unit =
    state = Text
    val content   = checkContent(reduce)
    def skipFirst = checkContent(content.substring(1))
    def skipBoth  = checkContent(content.substring(1, content.length - 1))

    content.charAt(0) match
      case '!' => // comment, so ignore
      case '&' => stack = UnescapedToken(skipFirst, otag, ctag) :: stack
      case '{' =>
        if content.endsWith("}") then
          stack = UnescapedToken(skipBoth, otag, ctag) :: stack
        else fail("Unbalanced \"{\" in tag \"" + content + "\"")
      case '^' =>
        stack = IncompleteSection(skipFirst, true, otag, ctag) :: stack
      case '#' =>
        stack = IncompleteSection(skipFirst, false, otag, ctag) :: stack
      case '/' =>
        val name = skipFirst

        @tailrec
        def addSection(children: List[Token], s: List[Token]): List[Token] =
          s.headOption match
            case None => fail("Closing unopend section \"" + name + "\"")
            case Some(IncompleteSection(key, inverted, startOTag, startCTag)) =>
              if key == name then
                SectionToken(
                  inverted,
                  name,
                  children,
                  startOTag,
                  startCTag,
                  otag,
                  ctag
                ) :: s.tail
              else fail("Unclosed section \"" + key + "\"")

            case Some(other) =>
              addSection(other :: children, s.tail)

        stack = addSection(List[Token](), stack)

      case '>' | '<' => stack = PartialToken(skipFirst, otag, ctag) :: stack
      case '=' =>
        if content.size > 2 && content.endsWith("=") then
          val changeDelimiter = skipBoth
          changeDelimiter.split("""\s+""", -1).toSeq match
            case Seq(o, c) =>
              stack = ChangeDelimitersToken(o, c, otag, ctag) :: stack
              otag = o
              ctag = c
            case _ =>
              fail(
                "Invalid change delimiter tag content: \"" + changeDelimiter + "\""
              )
        else fail("Invalid change delimiter tag content: \"" + content + "\"")

      case _ => stack = EscapedToken(content, otag, ctag) :: stack
