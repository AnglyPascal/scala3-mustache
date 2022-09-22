# Scala3-Mustache

This is a rewrite of [scala-mustache][4] for scala3 with some bug fixes.

## What is Mustache?

[Mustache][1] is a logic-free template engine inspired by ctemplate and et. 

As ctemplates says, "It emphasizes separating logic from presentation: it is impossible
to embed application logic in this template language".

To get the language-agnostic overview of Mustache's template syntax and examples of
Mustache templates, see <http://mustache.github.io/mustache.5.html>. 


## Usage

Using scala3-mustache is easy:
``` scala
import mustache.Mustache
...
val template1 = new Mustache("Hello, {{ name }}!")
template1.render(Map("name"->"world"))  // returns "Hello, world!"

val template2 = new Mustache("Boolean: {{#show}}{{ text }}{{/show}}!")
template2.render(Map("show" = true, "text" -> "Hello, world!")) // returns "Boolean: Hello, world!"

val template3 = new Mustache("Names: {{#names}}{{ name }} {{/name}}!")
template3.render(Map("names" -> List(Map("name" -> "A"), Map("name" -> "B")) // returns "Names: A B "
```

### Escaping 

By default, special characters inside the tag such as `<, >, ", &` are escaped in the
output:
``` scala
val template = new Mustache("Hello, {{ name }}!")
template.render(Map("name" -> "<b> world </b>")) 
```
will return
``` html
Hello, &lt;b&gt; world &lt;/b&gt;!
```
To prevent escaping of special characters, use `{{{tag}}}` or `{{&tag}}`
``` scala
val template = new Mustache("Hello, {{ &name }} {{{ tag }}}!")
template.render(Map("name" -> "<b> world </b>", "tag" -> "& everyone"))
```
returns,
``` html
Hello, <b> world </b> & everyone!
```

### Partials

It's possible to break up a mustache template, and call a template from within another
template. The inserted template is called a partial, and `{{> partial_name }}` is used
to insert the appropriate partial. 

To render a template containing partials, an optional `Map[String, Mustache]` might be
passed to the render function with the partials mapped from the names:

``` scala
val user = new Mustache("<strong>{{name}}</strong>")
val main = new Mustache("<h2>Names</h2>\n{{#names}}\n  {{> user}}\n{{/names}}")
val ctx =
  Map(
    "names" -> List(
      Map("name" -> "Alice"),
      Map("name" -> "Bob")
    )
  )
val partials = Map("user" -> user)
main.render(ctx, partials)
```
Which returns
```html
<h2>Names</h2>

  <strong>Alice</strong>

  <strong>Bob</strong>

```
Templates defined with partials can be thought of as a single expanded tempalte:
``` mustache
<h2>Names</h2>
{{#names}}
  <strong>{{name}}</strong>
{{/names}}
```

### Dot notation

You can use `{{.}}` and `{{{.}}}` to reference current context value:
``` scala
val template = new Mustache("{{#list}}{{.}} {{/list}}")
template.render(Map("list" -> List("alpha", "bravo", "charlie"))) // returns "alpha bravo charlie "
```

### Rendering with objects

When the context value is a callable object, such as a function or lambda, the object
will be invoked and passed the block of text. The text passed is the literal block,
unrendered. `{{tags}}` will not have been expanded - the lambda should do that on its
own. In this way you can implement filters or caching.
``` scala
val template = new Mustache("{{#wrapped}}{{name}} is awesome.{{/wrapped}}")
template.render(
  Map(
    "name" -> "Willy",
    "wrapped" -> ((str: String, render: String => String) => {
      "<b>" + render(str) + "</b>"
    })
  )
) // returns "<b>Willy is awesome.</b>"
```

Alternatively you can pack your helpers directly into the Mustache subclass. Following
example is effectively the same as previous:
``` scala
class MyMustache(template: String) extends Mustache(template):
  def wrapped(str: String) = "<b>" + render(str) + "</b>"

val template = new MyMustache("{{#wrapped}}{{name}} is awesome.{{/wrapped}}") 
template.render(Map("name" -> "Willy")) // returns "<b>Willy is awesome.</b>"
```

### Helper traits

Sometimes it is nice to keep different kinds of helpers separate. To do so, you can
define helper traits and then mix them up as needed:
``` scala
import mustache.MustacheHelperSupport

trait MyHelper:
  this: MyHelper with MustacheHelperSupport =>
  def wrapped(str: String) = "<b>" + render(str) + "</b>"

class MyMustache(template: String) 
  extends Mustache(template) with MyHelper
```
MustacheHelperSupport trait defines following methods you can use in your helper methods:
``` scala
protected def context: Any                      // returns current context
protected def render(template: String): String  // renders template string
```

[1]: https://mustache.github.io/
[2]: https://www.scala-sbt.org/
[3]: https://github.com/rallyhealth/weePickle/
[4]: https://github.com/vspy/scala-mustache
