import com.anglypascal.mustache.Parser
// import com.anglypascal.mustache.ASTs._
// import com.rallyhealth.weejson.v1._

import scala.io.Source

@main 
def hello: Unit =
  // val template = new Mustache("Hello, {{ #name }} {{{ tag }}} {{ /name }}!")
  // val n = new Parser()
  //
  val template = """
<h1>{{header}}</h1>
{{#bug}}
{{/bug}}

{{#items}}
  {{#first}}
    <li><strong>{{name}}</strong></li>
  {{/first}}
  {{#link}}
    <li><a href="{{url}}">{{name}}</a></li>
  {{/link}}
{{/items}}

{{#empty}}
  <p>The list is empty.</p>
{{/empty}}
  """ * 1000

  val tt = "aha" * 1000000
  val src = Source.fromString(tt)

  val p = new Parser(src, "{{", "}}")

  p.parse()
  println("done")
  // println(p.parse())

