package com.github.nau.jscala

object JScalaExample {
  def main(args: Array[String]) {
    object $ {
      def css() = this
    }

//    trait Node
//    trait Doc extends Node {
//      val doctype: DocumentType
//      val documentElement: Element


    val id = "id123"
    val js = javascript {
      def func1 = 1
      def func2() = ()
      def func3() = 2
      def func4(a: Int) = {
        val b = 5.0
        if (a > 2)
          (a + b) * 2
        else {
          val c = func3 * 2.0
          -c
        }
      }
      def func5(id: String) = {
        document.getElementById(id)
      }
      var a = 5.0
      val b = true
      val c = 8.8
      'a'
      +12
      -a
      !b
      (2 + a) * 3
      a = 6 - c
      func5(id)
      def func6(n: Int, f: Int => String) = f(n)
      func6(5, _.toString)
    }
    val js1 = javascript {
      val obj = new {
        val id = 8
        val payload = 5
      }
      obj.id
    }
    val js2 = javascript {
      def f() {
        while(true) return
      }
    }
    println(js1.eval())
    println(js2.print)
  }
}

