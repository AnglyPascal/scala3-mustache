import com.anglypascal.mustache._
import com.anglypascal.mustache.asts.CValue

import scala.io.Source
import com.rallyhealth.weejson.v1.{Value, Obj}

@main 
def hello: Unit =
  object CH extends ContextHandler
  object M extends Mustache("")
  import CValue.*

  type Partials = Map[String, Mustache]
  type CallStack = List[Any]
  type Render = String => String
  type Renderer = (Any, Partials, CallStack) => Render
  type TokenRender = (Any, Partials, CallStack) => TokenProduct

  // val map1  = valueToAST(Obj("a" -> "b"))
  val map2  = Obj("a" -> "b")

  def r1: Renderer = (_, _, _) => (_) => ""

  // val v1 = CH.valueOf("a", map1, Map(), List(M), "", r1)
  val v2 = CH.valueOf("a", map2, Map(), List(M), "", r1)
  // println(v1)
  println(v2)
