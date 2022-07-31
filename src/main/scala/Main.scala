import com.anglypascal.mustache._

import scala.io.Source

@main 
def hello: Unit =
  val template = new Mustache("Hello,{{ #name }} {{.}}{{ /name }}!")
  val l = Map("name" -> List(4, 2))
  println(template.render(l))

