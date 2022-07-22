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
import mustache._
...
val template1 = new Mustache("Hello, {{ name }}!")
template1.render(Map("name"->"world"))

val template2 = new Mustache("Boolean: {{#show}} {{ text }} {{/show}}!")
template2.render(Map("show" = true, "text" -> "Hello, world!"))

val template3 = new Mustache("Names: {{#names}}{{ name }} {{/name}}!")
template3.render(Map("names" -> List(Map("name" -> "A"), Map("name" -> "B"))
```
Returns:
``` html
Hello, world!
Boolean: Hello, world!
Names: A B
```
By default, special characters inside the tag such as `<, >, ", &` are escaped in
the output:
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


[1]: https://mustache.github.io/
[2]: https://www.scala-sbt.org/
[3]: https://github.com/rallyhealth/weePickle/
[4]: https://github.com/vspy/scala-mustache
