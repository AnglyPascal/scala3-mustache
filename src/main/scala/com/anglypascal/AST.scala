package com.anglypascal.mustache

import scala.collection.mutable.Set
import com.anglypascal.mustache.asts.CValue

trait AST:

  def findKey(key: String): Option[Any]

  def value: Any

object AST:

  private val converters = Set[ASTConverter]()

  def addConverter(conv: ASTConverter): Unit =
    converters += conv

  def findConverter(c: Any): Option[ASTConverter] =
    converters.filter(_.canHandle(c)).headOption

trait ASTConverter:

  def toAST(context: Any): Either[Any, AST]

  def canHandle(context: Any): Boolean

  AST.addConverter(this)
