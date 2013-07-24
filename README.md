JScala
======

Scala macro that produces JavaScript from Scala code.


Supported Features:
===================
* Variable definitions, basic unary and binary operations
* Named and anonymous functions
* Scala Arrays as Javascript Array literals
* Scala mutable/immutable Map as Javascript object
* Scala anonymous classes as Javascript objects
* if, while, for..in and for statements
* Global Javascript functions (parseInt etc)
* Basic Browser objects (window, history, location etc)
* Basic HTML DOM objects (Document, Element, Attribute, Node, NodeList etc)
* Raw Javascript inclusion
* Generated Javascript eval using Java ScriptEngine
* Pretty printing and compression using YUI compressor

Examples
========

This Scala code

```scala
val js = javascript {
  window.setTimeout(() => {
    val r = new RegExp("d.*", "g")
    def func(i: String) = r.exec(i)
    val list = document.getElementById("myList2")
    val map = collection.mutable.Map[String, String]()
    if (typeof(map) == "string") {
      for (idx <- 0 until list.attributes.length) {
        val attr = list.attributes.item(idx).asInstanceOf[Attribute]
        map(attr.name) = func(attr.textContent).asInstanceOf[String]
      }
    } else {
      val obj = new {
        val field = 1
        def func2(i: Int) = "string"
      }
      val links = Array("https://github.com/nau/scala")
      for (link <- links) {
        include("var raw = 'JavaScript'")
        console.log(link + obj.func2(obj.field))
      }
      window.location.href = links(0).replace("scala", "jscala")
    }
  }, 1000)
}
println(js.asString)
```

will print

```javascript
window.setTimeout((function () {
    var r = new RegExp("d.*", "g");
    function func(i) {
      return r.exec(i);
    };
    var list = document.getElementById("myList2");
    var map = {};
    if ((typeof(map)) == "string") for (var idx = 0; idx < (list.attributes.length); idx++) {
      var attr = list.attributes.item(idx);
      (map[attr.name]) = func(attr.textContent)
    } else {
      var obj = { field: 1,
                  func2: (function (i) {
                      return "string";
                    })};
      var links = ["https://github.com/nau/scala"];
      for (link in links) {
        var raw = 'JavaScript';
        console.log((link) + obj.func2(obj.field))
      };
      window.location.href = links[0].replace("scala", "jscala")
    }
  }), 1000)
```
      
How To Use
==========

In your build.sbt add

    resolvers += "nau-releases" at "http://nau.github.com/maven-repo/releases"

    libraryDependencies += "org.jscala" %% "jscala-macros" % "0.1"

    libraryDependencies += "com.yahoo.platform.yui" % "yuicompressor" % "2.4.7"
    
If you want to try the latest snapshot:

    resolvers += Resolver.sonatypeRepo("snapshots")

    libraryDependencies += "org.jscala" %% "jscala-macros" % "0.2-SNAPSHOT"

    libraryDependencies += "com.yahoo.platform.yui" % "yuicompressor" % "2.4.7"

In your code

```scala
import org.jscala._
val js = javascript { ... }
println(js.asString)
println(js.compress)
println(js.eval())
```
    
That's it!

Feedback
========

Any feedback is very welcome!

You can use [JScala mailing list](https://groups.google.com/forum/#!forum/jscala-user) if you have any questions.
