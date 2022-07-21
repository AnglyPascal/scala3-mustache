package com.anglypascal.mustache.ASTs
import com.anglypascal.mustache.{AST, Extensions}

import com.rallyhealth.weejson.v1._

abstract class CValue extends AST

class CObj(obj: Obj) extends CValue:
  def findKey(key: String): Option[AST] =
    import Extensions.*
    obj.obj.toMap.findKey(key).map(valueToAST)
  def value: Map[String, AST] =
    obj.obj.map(p => (p._1, valueToAST(p._2))).toMap

class CSeq(seq: Arr) extends CValue:
  def findKey(key: String): Option[Seq[AST]] = Some(value)
  def value: Seq[AST] =
    seq.arr.map(valueToAST).toSeq

class CStr(str: Str) extends CValue:
  def findKey(key: String): Option[String] = Some(str.str)
  def value: String                        = str.str

class CNum(num: Num) extends CValue:
  def findKey(key: String): Option[BigDecimal] = Some(value)
  def value: BigDecimal                        = num.num

class CBool(bool: Bool) extends CValue:
  def findKey(key: String): Option[Bool] = Some(value)
  def value: Boolean                     = bool.bool

object CNull extends CValue:
  def findKey(key: String): Option[Any] = None
  def value: Any                        = null

def valueToAST(value: Value): AST =
  value match
    case obj: Obj   => CObj(obj)
    case seq: Arr   => CSeq(seq)
    case str: Str   => CStr(str)
    case num: Num   => CNum(num)
    case bool: Bool => CBool(bool)
    case null       => CNull
    case other      => CNull

