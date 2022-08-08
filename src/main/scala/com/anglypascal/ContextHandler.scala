package com.anglypascal.mustache

import scala.annotation.tailrec
import scala.concurrent.{Awaitable, Await}
import scala.concurrent.duration.Duration
import java.lang.reflect.{Field, Method}
import com.anglypascal.mustache.asts.CValue

trait ContextHandler extends TypeAliases:
  protected def defaultRender(otag: String, ctag: String): Renderer =
    (context, partials, callstack) =>
      str => {
        val t = new Mustache(str, otag, ctag)
        t.render(context, partials, callstack)
      }

  def valueOf(
      key: String,
      context: Any,
      partials: Partials,
      callstack: CallStack,
      childrenString: String,
      render: Renderer
  ): Any =
    val r: Render = render(context, partials, callstack)

    val wrappedEval: () => Any = callstack
      .filter(_.isInstanceOf[Mustache])
      .asInstanceOf[List[Mustache]]
      .foldLeft(() => {
        eval(findInContext(context :: callstack, key), childrenString, r)
      })((f, e) => { () => { e.withContextAndRenderFn(context, r)(f()) } })

    wrappedEval() match
      case None if (key == ".") => context
      case other                => other

  @tailrec // add a way to extend this to match some other data structure, ie Value AST
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
      case other => other

  @tailrec
  private def findInContext(callstack: CallStack, key: String): Any =
    import Extensions.*
    callstack.headOption match
      case None => None
      case Some(head) =>
        (head match
          case null           => None
          case map: Map[?, ?] => map.findKey(key)
          case ctx: AST       => ctx.findKey(key)
          case other =>
            AST.findConverter(other) match
              case Some(conv) =>
                CValue.toAST(other) match
                  case Right(ast) => ast.findKey(key)
                  case Left(any)  => reflection(any, key)
              case None => reflection(other, key)
        ) match
          case None  => findInContext(callstack.tail, key)
          case other => other

  private def reflection(x: Any, key: String): Any =
    val w = x.asInstanceOf[AnyRef]
    methods(w).get(key) getOrElse (fields(w).get(key) getOrElse None)

  private def methods(w: AnyRef): Map[String, Object] =
    val excludedGlobals =
      List("wait", "toString", "hashCode", "getClass", "notify", "notifyAll")

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

    def mapMethod(x: Method): (String, Object) = {
      x.getName -> (
        if (x.getParameterTypes.length == 0) then () => { x.invoke(w) }
        else if (x.getParameterTypes.length == 1) then
          (str: String) => { x.invoke(w, str) }
        else (str: String, rnd: String => String) => { x.invoke(w, str, rnd) }
      )
    }

    Map(w.getClass.getMethods.filter(filterMethod).map(mapMethod).toSeq: _*)

  private def fields(w: AnyRef): Map[String, Field] = Map(
    w.getClass.getFields.map(x => { x.getName -> x }).toSeq: _*
  )
