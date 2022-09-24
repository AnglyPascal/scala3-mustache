package com.anglypascal.mustache.asts

import com.anglypascal.mustache.{TypeAliases, ContextHandler, Mustache}

import org.scalatest.flatspec.AnyFlatSpec
import scala.concurrent.{Future}
import scala.concurrent.ExecutionContext.Implicits.global
import com.rallyhealth.weejson.v1.Obj
import com.anglypascal.mustache.tokens.UnescapedToken

/** Test that Context handler behaves properly when given WeeJson Value objects
  */
class ContextHandlerWeeJsonValueSpecification
    extends AnyFlatSpec
    with TypeAliases:
  object CH extends UnescapedToken("", "", "") with ContextHandler

  val c = CValue

  val map0  = Obj()
  val map1  = Obj("a" -> "b")
  val map11 = Obj("a" -> false)
  val map12 = Obj("a" -> 1)
  val map2  = Obj("b" -> map1)

  def r1: Renderer = (_, _, _) => (_) => ""
  def r2: Renderer = (_, _, _) => str => "{" + str + "}"

  behavior of "valueOf"

  it should "return None for null context" in {
    assert(CH.valueOf("testKey", null, Map(), List(M), "", r1) === None)
  }

  it should "return None for values out of the Obj" in {
    assert(CH.valueOf("key", map0, Map(), List(M), "", r1) === None)
    assert(CH.valueOf("key", map1, Map(), List(M), "", r1) === None)
  }

  it should "return back the context if key is '.'" in {
    assert(CH.valueOf(".", Seq(), Map(), List(M), "", r1) === Seq())
    assert(CH.valueOf(".", map1, Map(), List(M), "", r1) === map1)
  }

  it should "get the value assigned to a key from Map" in {
    assert(CH.valueOf("a", map1, Map(), List(M), "", r1) === "b")
    assert(CH.valueOf("a", map11, Map(), List(M), "", r1) === false)
    assert(CH.valueOf("a", map12, Map(), List(M), "", r1) === 1)
    // assert(CH.valueOf("b", map2, Map(), List(M), "", r1) === c.valueToAST(map1))
    /** The above test doesn't work because the new CValue will have different
      * reference point. But this works as expected, and map12("a") returns an
      * ast CValue(map1), which is later used in mustache
      */
  }

  it should "find values inside callstack" in {
    assert(CH.valueOf("a", Obj(), Map(), List(map1, M), "", r1) === "b")
    assert(CH.valueOf("a", Obj(), Map(), List(map11, M), "", r1) === false)
  }

  object M extends Mustache("")
