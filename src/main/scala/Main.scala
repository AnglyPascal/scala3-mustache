import com.anglypascal.mustache.Mustache

@main 
def hello: Unit =
  val template = new Mustache("Hello, {{ &name }} {{{ tag }}}!")
  println(template.render(Map("name" -> "<b> world </b>", "tag" -> "& everyone")))
