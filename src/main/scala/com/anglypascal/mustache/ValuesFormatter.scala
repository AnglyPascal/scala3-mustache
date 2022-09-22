package com.anglypascal.mustache 

import scala.annotation.tailrec

object ValuesFormatter:
  @tailrec
  final def format(value: Any): String =
    value match
      case null => ""
      case None => ""
      case Some(v) => format(v)
      case x => x.toString
