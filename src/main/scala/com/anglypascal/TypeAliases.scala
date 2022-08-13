package com.anglypascal.mustache

import com.anglypascal.mustache.tokens.TokenProduct

trait TypeAliases:
  type Partials = Map[String, Mustache]
  type CallStack = List[Any]
  type Render = String => String
  type Renderer = (Any, Partials, CallStack) => Render
  type TokenRender = (Any, Partials, CallStack) => TokenProduct
