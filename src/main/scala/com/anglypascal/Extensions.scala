package com.anglypascal.mustache
import scala.reflect.TypeTest

extension [K, V](map: Map[K, V])
  def find(key: String)(using TypeTest[String, K]): Option[V] =
    key match
      case k: K => map.get(k)
      case _    => None

extension [A, B](fn: Function1[A, B])
  def applyAny(key: String)(using TypeTest[String, A]): Option[B] =
    key match
      case k: A => Some(fn(k))
      case _    => None

extension [A, B, C](fn: Function2[A, B, C])
  def applyAny(key: String, rnd: String => String)(using
      TypeTest[String, A],
      TypeTest[String => String, B]
  ): Option[C] =
    (key, rnd) match
      case (k: A, r: B) => Some(fn(k, r))
      case _            => None

