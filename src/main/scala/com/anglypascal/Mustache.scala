package com.anglypascal.mustache

import scala.io.Source

class Mustache(root: Token) extends MustacheHelperSupport:
  def this(source: Source, open: String = "{{", close: String = "}}") =
    this((new Parser(source, open, close)).parse())
  def this(str: String, open: String, close: String) =
    this(Source.fromString(str), open, close)
  def this(str: String) = this(Source.fromString(str))

  private val compiledTemplate: Token = root

  // val globals: Map[String, Any] =
  //   val excludedGlobals =
  //     List("wait", "toString", "hashCode", "getClass", "notify", "notifyAll")

  //   def filterMethod(x: java.lang.reflect.Method): Boolean =
  //     val name = x.getName
  //     val pt   = x.getParameterTypes

  //     !name.startsWith("render$default") &&
  //     !name.startsWith("product$default") &&
  //     !name.startsWith("init$default") &&
  //     !excludedGlobals.contains(name) &&
  //     ((pt.length == 0) || (pt.length == 1 && pt(0) == classOf[String]))

  //   def mapMethod(x: java.lang.reflect.Method): (String, Object) =
  //     x.getName -> (
  //       if x.getParameterTypes.length == 0 then () => { x.invoke(this) }
  //       else (str: String) => { x.invoke(this, str) }
  //     )

  //   Map((this.getClass.getMethods.filter(filterMethod).map(mapMethod)): _*)

  def render(
      context: Any = null,
      partials: Map[String, Mustache] = Map(),
      callstack: List[Any] = List(this)
  ): String =
    product(context, partials, callstack).toString

  def product(
      context: Any = null,
      partials: Map[String, Mustache] = Map(),
      callstack: List[Any] = List(this)
  ): TokenProduct =
    compiledTemplate.render(context, partials, callstack)
