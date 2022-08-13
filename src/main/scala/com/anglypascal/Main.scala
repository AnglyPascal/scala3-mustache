import com.anglypascal.mustache._
import com.anglypascal.mustache.asts.CValue

import scala.io.Source
import com.rallyhealth.weejson.v1.{Value, Obj}

@main
def hello: Unit =
  val user = new Mustache("<strong>{{name}}</strong>")
  val main = new Mustache(
    "<h2>Names</h2>\n{{#names}}\n  {{> user}}\n{{/names}}"
  )
  val ctx =
    Map(
      "names" -> List(
        Map("name" -> "Alice"),
        Map("name" -> "Bob")
      )
    )
  val partials = Map("user" -> user)
  val l        = main.render(ctx, partials)
  println(l)

