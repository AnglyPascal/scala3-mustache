package com.anglypascal.mustache

trait AST:
  def findKey(key: String): Option[Any]
  def value: Any
