package com.anglypascal.mustache

import org.scalatest.flatspec.AnyFlatSpec

import scala.concurrent.{Future}
import scala.concurrent.ExecutionContext.Implicits.global

class ContextHandlerSpecification extends AnyFlatSpec with TypeAliases:
  object CH extends ContextHandler

  val map0  = Map()
  val map1  = Map("a" -> "b")
  val map11 = Map("a" -> false)
  val map12 = Map("a" -> 1)
  val map1u = Map(1 -> 2)
  val map2  = Map("b" -> map1)

  def r1: Renderer = (_, _, _) => (_) => ""
  def r2: Renderer = (_, _, _) => str => "{" + str + "}"

  behavior of "valueOf"

  it should "return None for null context" in {
    assert(CH.valueOf("testKey", null, Map(), List(M), "", r1) === None)
  }

  it should "return None for values out of the map" in {
    assert(CH.valueOf("key", map0, Map(), List(M), "", r1) === None)
    assert(CH.valueOf("key", map1, Map(), List(M), "", r1) === None)
  }

  it should "return None for Map[K,V] where K is not >: string" in {
    assert(CH.valueOf("key", map1u, Map(), List(M), "", r1) === None)
    assert(CH.valueOf("key", map0, Map(), List(M), "", r1) === None)
  }

  it should "return None for unsupport AST" in {
    assert(CH.valueOf("key", Seq(), Map(), List(M), "", r1) === None)
    assert(
      CH.valueOf("key", Seq("a", "b"), Map(), List(M), "", r1) === None
    )
  }

  it should "return back the context if key is '.'" in {
    assert(CH.valueOf(".", Seq(), Map(), List(M), "", r1) === Seq())
    assert(CH.valueOf(".", map1, Map(), List(M), "", r1) === map1)
  }

  it should "get the value assigned to a key from Map" in {
    assert(CH.valueOf("a", map1, Map(), List(M), "", r1) === "b")
    assert(CH.valueOf("a", map11, Map(), List(M), "", r1) === false)
    assert(CH.valueOf("a", map12, Map(), List(M), "", r1) === 1)
    assert(CH.valueOf("b", map2, Map(), List(M), "", r1) === map1)
  }

  it should "find values inside callstack" in {
    assert(CH.valueOf("a", Map(), Map(), List(map1, M), "", r1) === "b")
    assert(CH.valueOf("a", Map(), Map(), List(map11, M), "", r1) === false)
  }

  val lam1 = () => "b"
  def lam2 = (str: String) => "b" + str + "c"
  val lam3 = (str: String, f: String => String) => "b" + f(str) + "c"
  def lam4 = (a: Int) => a
  val lam5 = (a: Int, f: String => String) => "b" + f(a.toString) + "c"

  it should "extract values out of closures and lambdas" in {
    assert(CH.valueOf("a", Map("a" -> lam1), Map(), List(M), "", r1) === "b")
    assert(CH.valueOf("a", Map("a" -> lam2), Map(), List(M), "", r1) === "bc")
    assert(CH.valueOf("a", Map("a" -> lam2), Map(), List(M), "s", r1) === "bsc")
    assert(
      CH.valueOf("a", Map("a" -> lam3), Map(), List(M), "cs", r2) === "b{cs}c"
    )
  }

  it should "fail to extract values out of lambdas with wrong type signature" in {
    assert(CH.valueOf("a", Map("a" -> lam4), Map(), List(M), "", r1) === None)
    assert(
      CH.valueOf("a", Map("a" -> lam5), Map(), List(M), "cs", r2) === None
    )
  }

  object O:
    val lV0                    = "local field"
    def lFn0                   = "local method"
    def lFn1: String => String = str => ">" + str + "<"

  it should "extract values from singleton objects" in {
    assert(CH.valueOf("lV0", O, Map(), List(M), "", r1) === "local field")
    assert(CH.valueOf("lFn0", O, Map(), List(M), "", r1) === "local method")
    assert(CH.valueOf("lFn1", O, Map(), List(M), "cs", r1) === ">cs<")
  }

  case class C(a: String, b: String)

  it should "extract values from case classes" in {
    val c = C("1", "2")
    assert(CH.valueOf("a", c, Map(), List(M), "", r1) === "1")
    assert(CH.valueOf("b", c, Map(), List(M), "", r1) === "2")
    assert(CH.valueOf("c", c, Map(), List(M), "", r1) === None)
  }

  object M extends Mustache(""):
    def gFn0 = "global value"
    def gFn1 = (str: String) => ">" + str + "<"

  it should "extract global values" in {
    assert(CH.valueOf("gFn0", null, Map(), List(M), "", r1) === "global value")
    assert(CH.valueOf("gFn1", null, Map(), List(M), "c", r1) === ">c<")
  }

  trait ST:
    val a: String
    def b = "b"

  object S extends ST:
    val a = "42"

  it should "extract values from traits" in {
    assert(CH.valueOf("a", S, Map(), List(M), "", r1) === "42")
    assert(CH.valueOf("b", S, Map(), List(M), "", r1) === "b")
    assert(CH.valueOf("c", S, Map(), List(M), "", r1) === None)
  }

  it should "extract values out of futures" in {
    assert(
      CH.valueOf("a", Map("a" -> Future { 42 }), Map(), List(M), "", r1) === 42
    )
    assert(
      CH.valueOf(
        "a",
        Map("a" -> Future { () => 42 }),
        Map(),
        List(M),
        "",
        r1
      ) === 42
    )
  }
