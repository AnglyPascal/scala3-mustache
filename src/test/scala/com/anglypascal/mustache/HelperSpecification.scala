package com.anglypascal.mustache

import org.scalatest.flatspec.AnyFlatSpec
import java.security.MessageDigest

class HelperSpecification extends AnyFlatSpec:

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
    val temp1 =
      """
      <ul>
        {{# users}}<li><img src="{{ gravatar }}">{{ login }}</li>
        {{/ users}}
      </ul>
      """
    val res1 =
      """
      <ul>
        <li><img src="https://secure.gravatar.com/avatar0784394d110aab7e786177a3b688ab0b?s=30">alice</li>
        <li><img src="https://secure.gravatar.com/avatar91c91fb8529ae2efcd2f58791533ecca?s=30">bob</li>
        
      </ul>
      """
    val g = new GravatarMustacheExample(true, temp1)
    val m = Map(
      "users" -> List(
        Map("email" -> "alice@example.org", "login" -> "alice"),
        Map("email" -> "bob@example.org", "login"   -> "bob")
      )
    )
    val s = g.render(m).toString
    assert(s === res1)
  }

  it should "render values returned by parent helper" in {
    val temp1 =
      """
          <ul>
            {{# users}}<li><img src="{{ gravatar }}">{{ login }}</li>
            {{/ users}}
          </ul>
      """
    val temp2 =
      """
      <html>
        <body>
          {{> userList }}
        </body>
      </html>
      """
    val res1 =
      """
      <html>
        <body>
          
          <ul>
            <li><img src="https://secure.gravatar.com/avatar0784394d110aab7e786177a3b688ab0b?s=30">alice</li>
            <li><img src="https://secure.gravatar.com/avatar91c91fb8529ae2efcd2f58791533ecca?s=30">bob</li>
            
          </ul>
      
        </body>
      </html>
      """
    val userList = new Mustache(temp1)
    val page     = new Mustache(temp2)
    val root     = new GravatarMustacheExample(true, "{{>content}}")

    val map = Map(
      "users" -> List(
        Map("email" -> "alice@example.org", "login" -> "alice"),
        Map("email" -> "bob@example.org", "login"   -> "bob")
      )
    )
    val s = root.render(map, Map("content" -> page, "userList" -> userList))
    assert(s === res1)
  }
