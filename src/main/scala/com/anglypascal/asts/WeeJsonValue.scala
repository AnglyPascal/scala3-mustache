package com.anglypascal.mustache.asts

import com.anglypascal.mustache.{AST, Extensions, ASTConverter}

import scala.language.implicitConversions
import com.rallyhealth.weejson.v1.{Value, Obj, Str, Num, Bool, Arr, Null}

class CValue(v: Value) extends AST:
  def findKey(key: String): Option[Any] =
    v match
      case obj: Obj =>
        import Extensions.*
        obj.obj.toMap.findKey(key).map(CValue.valueToAST)
      case null     => None
      case other    => Some(value)

  def value: Any =
    v match
      case obj: Obj   => obj.obj.map(p => (p._1, CValue.valueToAST(p._2))).toMap
      case str: Str   => str.str
      case num: Num   => num.num
      case bool: Bool => bool.bool
      case arr: Arr   => arr.arr.map(CValue.valueToAST).toSeq
      case _          => None

object CValue extends ASTConverter:

  implicit def valueToAST(value: Value): AST = new CValue(value)

  def toAST(context: Any): Either[Any, AST] = 
    context match 
      case c: Value => Right(c)
      case other => Left(other)

  def canHandle(context: Any): Boolean = 
    context match
      case c: Value => true
      case _ => false

