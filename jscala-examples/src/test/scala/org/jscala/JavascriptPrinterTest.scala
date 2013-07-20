package org.jscala

import org.scalatest.FunSuite
import org.jscala.{javascript=>js}

class JavascriptPrinterTest extends FunSuite {
  test("Printer") {
    val ast = js {
      val a = Array("1", "2", "3")
      for (i <- a) console.log(i)
    }
    assert(ast.asString === """{
                            |  var a = ["1", "2", "3"];
                            |  for (i in a) console.log(i)
                            |}""".stripMargin)
  }

  test("IIFE") {
    val ast = js {
      (() => {
        console.log(1)
        ()
      })()
    }
    assert(ast.asString === """(function () {
                            |  console.log(1)
                            |})()""".stripMargin)
  }

  test("YUI Compressor") {
    val ast = js {
      val a = Array("1", "2", "3")
      for (i <- a) console.log(i)
    }
    assert(ast.compress === """var a=["1","2","3"];
                           |for(i in a){console.log(i)
                           |};""".stripMargin)
  }
}
