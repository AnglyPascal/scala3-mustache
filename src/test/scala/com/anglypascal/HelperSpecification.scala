package com.anglypascal.mustache

import org.scalatest.flatspec.AnyFlatSpec
import java.security.MessageDigest

object HelperSpecification extends AnyFlatSpec:

  object MD5:
    def apply(key: String): String =
      val bytes = key.getBytes("UTF-8")
      val md5   = MessageDigest.getInstance("MD5")
      md5.reset()
      md5.update(bytes)
      md5.digest
        .map(0xff & _)
        .map("%02x".format(_))
        .mkString

  /** Provides the gravatar function which takes an email address and returns
    * the gravatar id for that email
    *
    * This gravatar function with type () => String is used in mustache
    * rendering
    */
  trait GravatarHelper:
    this: GravatarHelper with MustacheHelperSupport =>

    import Extensions.*

    def gravatar =
      context match
        case m: Map[?, ?] =>
          gravatarForID(MD5(m.findKey("email").toString.trim.toLowerCase))
        case _ =>
          throw new IllegalArgumentException(
            "Invalid context for gravatar rendering: " + context
          )
    private def gravatarForID(gid: String, size: Int = 30) =
      gravatarHost + "/avatar" + gid + "?s=" + size

    private def gravatarHost =
      if (ssl) then "https://secure.gravatar.com"
      else "http://www.gravatar.com"

    protected val ssl: Boolean

  /** The class to be used in this testing */
  class GravatarMustacheExample(isSsl: Boolean, template: String)
      extends Mustache(template)
      with GravatarHelper:
    val ssl = isSsl

  behavior of "mustache"

  it should "render values returned by helper" in {
    val g = new GravatarMustacheExample(
      true,
      """
      <ul>{{# users}}<li><img src=\"{{ gravatar }}\">{{ login }}</li>{{/ users}}</ul>
      """
    )
    val m = Map(
      "users" -> List(
        Map("email" -> "alice@example.org", "login" -> "alice"),
        Map("email" -> "bob@example.org", "login"   -> "bob")
      )
    )
    val s = g.render(m).toString
    assert(
      s === ("""<ul>""" +
        """<li><img src="https://secure.gravatar.com/avatar/""" +
        """fbf7c6aec1d4280b7c2704c1c0478bd6?s=30">alice</li>""" +
        """<li><img src="https://secure.gravatar.com/avatar/""" +
        """10ac39056a4b6f1f6804d724518ff2dc?s=30">bob</li>""" +
        """</ul>""")
    )
  }

  it should "render values returned by parent helper" in {
    val userList = new Mustache(
      """
      <ul>{{# users}}<li><img src=\"{{ gravatar }}\">{{ login }}</li>{{/ users}}</ul>
      """
    )
    val page = new Mustache("<html><body>{{>userList}}</body></html>")
    val root = new GravatarMustacheExample(true, "{{>content}}")
    val result =
      """<html><body><ul>""" +
        """<li><img src="https://secure.gravatar.com/avatar/""" +
        """fbf7c6aec1d4280b7c2704c1c0478bd6?s=30">alice</li>""" +
        """<li><img src="https://secure.gravatar.com/avatar/""" +
        """10ac39056a4b6f1f6804d724518ff2dc?s=30">bob</li>""" +
        """</ul></body></html>"""

    val map = Map(
      "users" -> List(
        Map("email" -> "alice@example.org", "login" -> "alice"),
        Map("email" -> "bob@example.org", "login"   -> "bob")
      )
    )

    val s = root.render(map, Map("content" -> page, "userList" -> userList))
    assert(s === result)
  }
