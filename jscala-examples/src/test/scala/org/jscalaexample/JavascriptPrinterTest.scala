package org.jscalaexample

import org.scalatest.FunSuite
import org.jscala._
import org.jscala.{javascript=>js}

class JavascriptPrinterTest extends FunSuite {
  test("Printer") {
    val ast = js {
      val a = Array("1", "2", "3")
      for (i <- a) console.log(i)
    }
    assert(ast.asString === """{
                            |  var a = ["1", "2", "3"];
                            |  for (i in a) console.log(i);
                            |}""".stripMargin)
  }

  test("IIFE") {
    val ast = js {
      (() => {
        val a = 1
        console.log(a)
      })()
    }
    assert(ast.asString === """(function () {
                              |    var a = 1;
                              |    console.log(a);
                              |  })()""".stripMargin)
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
