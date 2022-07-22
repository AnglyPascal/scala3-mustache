// package com.anglypascal.mustache
// import com.anglypascal.mustache.tokens._

// import scala.io.Source
// import scala.annotation.tailrec

// case class State(
//     ps: ParserState,
//     tagPos: Int,
//     line: Int,
//     prev: Char,
//     buf: String,
//     curlyBraceTag: Boolean,
//     stack: List[Token]
// )

// val emptyState = State(Text, 0, 0, '\uffff', "", false, List())

// def charParse(otag: String, ctag: String)(state: State, ch: Char): State =
//   val State(ps, tp, l, p, buf, cbt, stack) = state
//   if ch == '\r' || (ch == '\n' && state.prev != '\r') then
//     State(ps, tp, l + 1, ch, buf, cbt, stack)
//   else
//     ps match
//       case Text => fromText(otag, ctag)(ch, state)
//       case OTag => fromOTag(otag, ctag)(ch, state)
//       case Tag  => fromTag(otag, ctag)(ch, state)
//       case CTag => fromCTag(otag, ctag)(ch, state)
//       case _    => state

// def staticText(buf: String, stack: List[Token]): List[Token] =
//   if buf.length > 0 then StaticTextToken(buf) :: stack else stack

// /** need to change p to ch
//   */
// def fromText(otag: String, ctag: String)(ch: Char, state: State): State =
//   val State(ps, tp, l, p, buf, cbt, stack) = state
//   if ps != Text then state
//   else if ch == otag.head then
//     if otag.length > 1 then State(OTag, tp + 1, l, ch, buf, cbt, stack)
//     else State(Tag, tp, l, ch, "", cbt, staticText(buf, stack))
//   else State(ps, tp, l, ch, buf + ch, cbt, stack)

// def fromOTag(otag: String, ctag: String)(ch: Char, state: State): State =
//   val State(ps, tp, l, p, buf, cbt, stack) = state
//   if ps != OTag then state
//   else if ch == otag.charAt(tp) then
//     if tp == otag.length - 1 then
//       State(Tag, tp, l, ch, "", cbt, staticText(buf, stack))
//     else State(ps, tp + 1, l, ch, buf, cbt, stack)
//   else State(Text, tp, l, ch, buf + otag.substring(0, tp) + ch, cbt, stack)

// def fromTag(otag: String, ctag: String)(ch: Char, state: State): State =
//   val State(ps, tp, l, p, buf, cbt, stack) = state
//   if ps != Tag then state
//   else if buf.isEmpty && ch == '{' then
//     State(ps, tp, l, ch, buf + ch, true, stack)
//   else if cbt && ch == '}' then State(ps, tp, l, ch, buf + ch, false, stack)
//   else if ch == ctag.head then
//     if ctag.length > 1 then State(CTag, tp + 1, l, ch, buf, cbt, stack)
//     else State(Text, tp, l, ch, "", cbt, tagText(buf, stack))
//   else State(Tag, tp, l, ch, buf + ch, cbt, stack)

// def fromCTag(otag: String, ctag: String)(ch: Char, state: State): State =
//   val State(ps, tp, l, p, buf, cbt, stack) = state
//   if ps != CTag then state
//   else if ch == ctag.charAt(tp) then
//     if tp == ctag.length - 1 then
//       State(Text, tp, l, ch, "", cbt, tagText(buf, stack))
//     else State(ps, tp + 1, l, ch, buf, cbt, stack)
//   else State(Tag, tp, l, ch, buf + ctag.substring(0, tp) + ch, cbt, stack)

// def tagText(buf: String, stack: List[Token]): List[Token] = ???

// /** Problem with this implementation:
//  *  - how do you handle the error?
//  *  - this code is horrible to look at (1) and is very difficult to maintain
//  *  - there seems to be a lot of reproducible behavior, can it not be modularized
//  *  further?
//  */


// def srcParse(otag: String, ctag: String)(src: Source): State =
//   src.foldLeft(emptyState)(charParse(otag, ctag)(_, _))
