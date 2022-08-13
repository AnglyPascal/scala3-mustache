package com.anglypascal.mustache
import scala.reflect.{TypeTest, Typeable}

object Extensions:
  extension [K, V](map: Map[K, V])
    def findKey(key: String)(using tt: TypeTest[String, K]): Option[V] =
      key match
        case k: K => map.get(k)
        case _    => None

  extension [A, B](fn: Function1[A, B])
    def applyAny(key: String)(using tt: TypeTest[String, A]): Option[B] =
      key match
        case key: A => 
          try Some(fn(key))
          catch 
            case jlc: java.lang.ClassCastException => None
            case e => throw e
        case _    => None

  extension [A, B, C](fn: Function2[A, B, C])
    def applyAny(key: String, rnd: String => String)(using
        TypeTest[String, A],
        TypeTest[String => String, B]
    ): Option[C] =
      (key, rnd) match
        case (k: A, r: B) => 
          try Some(fn(k, r))          
          catch 
            case jlc: java.lang.ClassCastException => None
            case e => throw e
        case _            => None
