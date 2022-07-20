package com.anglypascal.mustache

import scala.collection.mutable.StringBuilder

trait TokenProduct:
  val maxLength: Int 
  def write(out: StringBuilder): Unit

  override def toString =
    val b = new StringBuilder(maxLength)
    write(b)
    b.toString

object EmptyProduct extends TokenProduct:
  val maxLength: Int = 0
  def write(out: StringBuilder): Unit = {}

case class StringProduct(str: String) extends TokenProduct:
  val maxLength: Int = str.length
  def write(out: StringBuilder): Unit = out.append(str)
  
