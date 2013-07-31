package org.jscalaexample

import org.jscala._

object JScalaExample {
  def domManipulations() {
    val html = javascript {
      val node = document.getElementById("myList2").lastChild
      document.getElementById("myList1").appendChild(node)
      val buttons = document.getElementsByTagName("link")
      for (idx <- 0 until buttons.length) {
        console.log(buttons.item(idx).attributes)
      }
    }.asString
    println(html)
  }

  def browserStuff() {
    val js = javascript {
      window.location.href = "http://jscala.org"
      window.open("https://github.com")
      history.back()
    }
    println(js.asString)
  }

  def complexExample() {
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
  }

  def main(args: Array[String]) {
    domManipulations()
    browserStuff()
    complexExample()
  }
}

