import com.anglypascal.mustache.Mustache

@main 
def hello: Unit =
  val mus = new Mustache("{{{}}}")
  println(mus.render())
