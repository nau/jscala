package com.github.nau.jscala

import org.scalatest.FunSuite
import com.github.nau.jscala.{javascript=>js}

class JavascriptPrinterTest extends FunSuite {
  test("Printer") {
    val ast = js {
      val a = Array("1", "2", "3")
      for (i <- a) console.log(i)
    }
    assert(ast.print === """{
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
    assert(ast.print === """(function () {
                            |  console.log(1)
                            |})()""".stripMargin)
  }
}
