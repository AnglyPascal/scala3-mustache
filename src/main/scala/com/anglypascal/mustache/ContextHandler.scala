package com.anglypascal.mustache

import com.anglypascal.mustache.tokens.Token

import java.lang.reflect.Field
import java.lang.reflect.Method
import scala.annotation.tailrec
import scala.concurrent.Await
import scala.concurrent.Awaitable
import scala.concurrent.duration.Duration

trait ContextHandler extends TypeAliases:
  this: Token =>

  def valueOf(
      key: String,
      context: Any,
      partials: Partials,
      callstack: CallStack,
      childrenString: String,
      render: Renderer
  ): Any =
    val r: Render = render(context, partials, callstack)

    val wrappedEval: () => Any =
      callstack
        .filter(_.isInstanceOf[Mustache])
        .asInstanceOf[List[Mustache]]
        .foldLeft(() => {
          eval(findInContext(context :: callstack, key), childrenString, r)
        })((f, e) => { () => { e.withContextAndRenderFn(context, r)(f()) } })

    wrappedEval() match
      case None if (key == ".") => context
      case other                => other

  @tailrec
  private def eval(value: Any, str: String, render: Render): Any =
    import Extensions.*
    import AST.*
    value match
      case Some(someVal)  => eval(someVal, str, render)
      case seq: Seq[?]    => seq
      case map: Map[?, ?] => map
      case ctx: AST       => ctx.value
      case a: Awaitable[?] =>
        eval(Await.result(a, Duration.Inf), str, render)
      case f0: Function0[?] => eval(f0(), str, render)
      case f1: Function1[?, ?] =>
        eval(f1.applyAny(str), str, render)
      case f2: Function2[?, ?, ?] =>
        eval(f2.applyAny(str, render), str, render)
      case other =>
        AST.findConverter(other) match
          case Some(conv) =>
            conv.toAST(other) match
              case Right(ast) => ast.value
              case Left(any)  => other
          case None => other

  @tailrec
  private def findInContext(callstack: CallStack, key: String): Any =
    import Extensions.*
    callstack.headOption match
      case None => None
      case Some(head) =>
        val v = head match
          case null           => None
          case map: Map[?, ?] => map.findKey(key)
          /** findKey also needs to check for attributes like length or such? */
          case ctx: AST => ctx.findKey(key)
          case other =>
            AST.findConverter(other) match
              case Some(conv) =>
                conv.toAST(other) match
                  case Right(ast) => ast.findKey(key)
                  case Left(any)  => reflection(any, key)
              case None => reflection(other, key)

        v match
          case None  => findInContext(callstack.tail, key)
          case other => other

  private inline def reflection(x: Any, key: String): Any =
    val w = x.asInstanceOf[AnyRef]
    methods(w).get(key) getOrElse (fields(w).get(key) getOrElse None)

  private val excludedGlobals =
    List("wait", "toString", "hashCode", "getClass", "notify", "notifyAll")

  private inline def methods(w: AnyRef): Map[String, Object] =
    def filterMethod(x: Method): Boolean =
      val name   = x.getName
      val params = x.getParameterTypes

      !name.startsWith("render$default") &&
      !name.startsWith("product$default") &&
      !name.startsWith("init$default") &&
      !excludedGlobals.contains(name) &&
      ((params.length == 0) ||
        (params.length == 1 &&
          params(0) == classOf[String]) ||
        (params.length == 2 &&
          params(0) == classOf[String] &&
          params(1) == classOf[Function1[String, String]]))

    def mapMethod(x: Method): (String, Object) =
      x.getName -> (
        if (x.getParameterTypes.length == 0) then () => { x.invoke(w) }
        else if (x.getParameterTypes.length == 1) then
          (str: String) => { x.invoke(w, str) }
        else (str: String, rnd: String => String) => { x.invoke(w, str, rnd) }
      )

    w.getClass.getMethods
      .collect(method =>
        filterMethod(method) match
          case true => mapMethod(method)
      )
      .toMap

  private inline def fields(w: AnyRef): Map[String, Field] =
    w.getClass.getFields.map(x => x.getName -> x).toMap
