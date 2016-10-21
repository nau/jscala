JScala
======

Scala macro that produces JavaScript from Scala code. Let it be type safe!

[![Build Status](https://secure.travis-ci.org/nau/jscala.png)](http://travis-ci.org/nau/jscala)

[![Gitter](https://badges.gitter.im/nau/jscala.svg)](https://gitter.im/nau/jscala?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge)

Supported Features:
===================
* Variable definitions, basic unary and binary operations
* Named and anonymous functions
* Scala Arrays/Seq as JavaScript Array literals
* Scala Map and anonymous classes as JavaScript object
* if, while, for..in and for statements
* Scala if as an expression (e.g. val a = if (true) 1 else 2)
* Scala match as JavaScript switch
* Basic Scala class/trait definition to JavaScript object definition translation
* Global JavaScript functions (parseInt etc)
* Basic Browser objects (window, history, location etc)
* Basic HTML DOM objects (Document, Element, Attribute, Node, NodeList etc)
* Raw JavaScript inclusion
* Values and function call injections from your Scala code
* Generated JavaScript eval using Java ScriptEngine
* Pretty printing and compression using YUI compressor
* Basic @Javascript macro annotation support
* Basic toJson/fromJson macros
* @Typescripted annotation

Examples
========

This Scala code has no meaning but shows basic ideas:

```scala
val replacement = "text"
val js = javascript {
  window.setTimeout(() => {
    val r = new RegExp("d.*", "g")
    class Point(val x: Int, val y: Int)
    val point = new Point(1, 2)
    def func(i: String) = r.exec(i)
    val list = document.getElementById("myList2")
    val map = collection.mutable.Map[String, String]()
    if (typeof(map) == "string") {
      for (idx <- 0 until list.attributes.length) {
        val attr = list.attributes.item(idx).asInstanceOf[Attribute]
        map(attr.name) = func(attr.textContent).as[String]
      }
    } else {
      val obj = new {
        val field = 1
        def func2(i: Int) = "string"
      }
      val links = Array("https://github.com/nau/scala")
      for (link <- links) {
        include("var raw = 'JavaScript'")
        console.log(link + obj.func2(obj.field) + point.x)
      }
      window.location.href = links(0).replace("scala", "jscala")
    }
  }, 1000)
}
println(js.asString)
```

It will print

```javascript
window.setTimeout(function () {
    var r = new RegExp("d.*", "g");
    function Point(x, y) {
      this.x = x;
      this.y = y;
    };
    var point = new Point(1, 2);
    function func(i) {
      return r.exec(i);
    };
    var list = document.getElementById("myList2");
    var map = {};
    if (typeof(map) == "string") for (var idx = 0; idx < list.attributes.length; ++idx) {
      var attr = list.attributes.item(idx);
      map[attr.name] = func(attr.textContent);
    } else {
      var obj = {
        field: 1,
        func2: function (i) {
          return "string";
        }
      };
      var links = ["https://github.com/nau/scala"];
      for (var linkIdx = 0, link = links[linkIdx]; linkIdx < links.length; link = links[++linkIdx]) {
        var raw = 'JavaScript';
        console.log((link + obj.func2(obj.field)) + point.x);
      };
      window.location.href = links[0].replace("scala", "jscala");
    };
  }, 1000)
```
      
How To Use
==========

In your build.sbt add

    scalaVersion := "2.10.6" // or "2.11.8"

    libraryDependencies += "org.jscala" %% "jscala-macros" % "0.4"
    
If you want to try the latest snapshot:

    scalaVersion := "2.11.8"

    resolvers += Resolver.sonatypeRepo("snapshots")

    libraryDependencies += "org.jscala" %% "jscala-macros" % "0.5-SNAPSHOT"

In your code

```scala
import org.jscala._
val js = javascript { ... }
println(js.asString)
println(js.compress)
println(js.eval())
```
    
That's it!

How To Try Macro Annotations
============================
In your build.sbt add

    scalaVersion := "2.11.8"

    resolvers += Resolver.sonatypeRepo("snapshots")
    
    addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)

    libraryDependencies += "org.jscala" %% "jscala-macros" % "0.3"
    
    libraryDependencies += "org.jscala" %% "jscala-annots" % "0.3"

In your code

```scala
import org.jscala._
@Javascript class User(val name: String, val id: Int)
@Javascript(json = false) class Greeter {
  def hello(u: User) {
    print("Hello, " + u.name + "\n")
  }
}
// Run on JVM
val u1 = new User("Alex", 1)
val greeter = new Greeter()
greeter.hello(u1) // prints "Hello, Alex"
val json = u1.js.json.asString
val main = javascript {
    val u = new User("nau", 2)
    val u1Json = eval("(" + inject(json) + ")").as[User] // read User from json string generated above
    val t = new Greeter()
    t.hello(u)
    t.hello(u1Json)
  }
val js = User.jscala.javascript ++ Greeter.jscala.javascript ++ main // join classes definitions with main code
js.eval() // run using Rhino
println(js.asString) // prints resulting JavaScript
```

Run it and you'll get

```javascript
Hello, Alex

Hello, nau
Hello, Alex
{
  function User(name, id) {
    this.name = name;
    this.id = id;
  };
  function Greeter() {
    this.hello = function (u) {
      print(("Hello, " + u.name) + "\n");
    };
  };
  var u = new User("nau", 2);
  var u1Json = eval(("(" + "{\n  \"name\": \"Alex\",\n  \"id\": 1\n}") + ")");
  var t = new Greeter();
  t.hello(u);
  t.hello(u1Json);
}
```

See AES example:

https://github.com/nau/jscala/blob/master/jscala-examples/src/main/scala/org/jscalaexample/AES.scala

It's AES Scala implementation which is used for both Scala and JavaScript encryption/decryption.


How To Build And Play Some Tetris
=================================

Make sure you have at least -Xmx750Mb for your sbt. 
Don't know why but it takes up to 700Mb to compile _jscala-macros_ project.

In sbt shell run `tetris` task. 
It will compile and generate _tetris.js_ file in _jscala-examples/javascript-tetris_ and open Tetris game in your browser.
Tetris is fully written in Scala and translates to JavaScript mostly literally. 

Tetris sources are here: [jscala-examples/src/main/scala/org/jscalaexample/Tetris.scala](https://github.com/nau/jscala/blob/master/jscala-examples/src/main/scala/org/jscalaexample/Tetris.scala)

Planned Features
================

* Language support improvements
* Web frameworks support: Play, Lift

Feedback
========

Any feedback is very welcome!

You can use [JScala mailing list](https://groups.google.com/forum/#!forum/jscala-user) if you have any questions.

Or simply ask me on Twitter: [@atlanter](https://twitter.com/atlanter)
