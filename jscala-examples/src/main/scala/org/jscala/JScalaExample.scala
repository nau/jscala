package org.jscala

object JScalaExample {
  def domManipulations() {
    val html = <script>{ javascript {
      val node = document.getElementById("myList2").lastChild
      document.getElementById("myList1").appendChild(node)
      val buttons = document.getElementsByTagName("link")
      for (idx <- 0 until buttons.length) {
        console.log(buttons.item(idx).attributes)
      }
    }.asString}</script>
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

  def main(args: Array[String]) {
    domManipulations()
    browserStuff()
  }
}

