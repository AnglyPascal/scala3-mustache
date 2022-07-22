package com.anglypascal.mustache.ASTs
import com.anglypascal.mustache.{AST, Extensions}

import scala.language.implicitConversions
import com.rallyhealth.weejson.v1.{Value, Obj, Str, Num, Bool, Arr, Null}

case class CValue(v: Value) extends AST:
  def findKey(key: String): Option[Any] =
    v match
      case obj: Obj =>
        import Extensions.*
        obj.obj.toMap.findKey(key).map(valueToAST)
      case arr: Arr => Some(arr.arr.toSeq)
      case null     => None
      case other    => Some(value)

  def value: Any =
    v match
      case obj: Obj   => obj.obj.map(p => (p._1, valueToAST(p._2))).toMap
      case str: Str   => str.str
      case num: Num   => num.num
      case bool: Bool => bool.bool
      case arr: Arr   => arr.arr.map(valueToAST).toSeq
      case _          => None

implicit def valueToAST(value: Value): AST = CValue(value)
