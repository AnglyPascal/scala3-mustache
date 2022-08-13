package com.anglypascal.mustache.tokens

import org.scalatest.flatspec.AnyFlatSpec
import com.anglypascal.mustache.Mustache

class SectionTokenSpecification extends AnyFlatSpec:

  object M extends Mustache("")
  def main(children: List[Token], inv: Boolean = false) =
    SectionToken(inv, "main", children, "{{", "}}", "{{", "}}")
  val t1 = StaticTextToken("fooBar")
  val t2 = EscapedToken("name", "{{", "}}")
  val t3 = StaticTextToken(",")

  it should "not render children with null or None context" in {
    val str1 = main(List(t1)).render(null, Map(), List(M)).toString
    val str2 = main(List(t1)).render(None, Map(), List(M)).toString
    assert(str1 === "" && str2 == "")
  }

  it should "render children with invert null or None context" in {
    val str1 = main(List(t1), true).render(null, Map(), List(M)).toString
    val str2 = main(List(t1), true).render(None, Map(), List(M)).toString
    assert(str1 === "fooBar" && str2 == "fooBar")
  }

  it should "handle boolean context" in {
    val str1 =
      main(List(t1)).render(Map("main" -> true), Map(), List(M)).toString
    val str2 =
      main(List(t1)).render(Map("main" -> false), Map(), List(M)).toString
    assert(str1 === "fooBar")
    assert(str2 === "")
  }

  it should "handle inverse boolean context" in {
    val str1 =
      main(List(t1), true).render(Map("main" -> true), Map(), List(M)).toString
    val str2 =
      main(List(t1), true).render(Map("main" -> false), Map(), List(M)).toString
    assert(str1 === "")
    assert(str2 === "fooBar")
  }

  it should "handle list context" in {
    val str1 =
      main(List(t2)).render(Map("main" -> List()), Map(), List(M)).toString
    val str2 = main(List(t2, t3))
      .render(
        Map("main" -> List(Map("name" -> "A"), Map("name" -> "B"))),
        Map(),
        List(M)
      )
      .toString
    assert(str1 === "")
    assert(str2 === "A,B,")
  }

  it should "handle inverse list context" in {
    val str1 =
      main(List(t1), true)
        .render(Map("main" -> List()), Map(), List(M))
        .toString
    val str2 =
      main(List(t2, t3), true)
        .render(
          Map("main" -> List(Map("name" -> "A"), Map("name" -> "B"))),
          Map(),
          List(M)
        )
        .toString
    assert(str1 === "fooBar")
    assert(str2 === "")
  }
