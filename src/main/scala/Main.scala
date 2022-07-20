import com.anglypascal.mustache.Mustache

@main 
def hello: Unit =
  val mus = new Mustache("{{#foo}}{{value}}{{/foo}}"){
    def foo(str: String) = "<b>" + str + "</b>"
  }

  val s = mus
    .render(
      // Map("foo" -> List(Map("value" -> "bar"), Map("value" -> "foo")))
    )
    .toString

  println(s)
