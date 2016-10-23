package org.jscalaexample

import org.scalatest.FunSuite
import org.jscala._
import org.jscala.{javascript=>js}
import org.scalajs.dom.console

class JavascriptPrinterTest extends FunSuite {
  test("String escaping") {
    val ast = js {
      val quote1 = """&copy; "something""""
      val quote2 = "&copy; \"something\""
      val multiline1 = "a\nb\tc"
      val multiline2 =
        """a
          b
          c"""
    }
    assert(ast.asString ===
      """{
        |  var quote1 = "&copy; \"something\"";
        |  var quote2 = "&copy; \"something\"";
        |  var multiline1 = "a\nb\tc";
        |  var multiline2 = "a\n          b\n          c";
        |}""".stripMargin)
    ast.eval()
  }

  test("Printer") {
    val ast = js {
      val a = Array("1", "2", "3")
      for (i <- a) console.log(i)
    }
    assert(ast.asString === """{
                            |  var a = ["1", "2", "3"];
                            |  for (var iColl = a, iIdx = 0, i = iColl[iIdx]; iIdx < iColl.length; i = iColl[++iIdx]) console.log(i);
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
                           |for(var iColl=a,iIdx=0,i=iColl[iIdx];
                           |iIdx<iColl.length;
                           |i=iColl[++iIdx]){console.log(i)
                           |};""".stripMargin)
  }

  test("Ternary operator") {
    val ast = js { if ((Math.PI > 3) && (Math.PI < 4)) Math.PI else Math.E }
    assert(ast.asString === "((Math.PI > 3) && (Math.PI < 4)) ? Math.PI : Math.E")
  }

  test("Switches") {
    val ast = js {
      val a = 1 match {
        case 1 | 2 => 1
      }
    }
    assert(ast.asString ===
      """{
        |  var a;
        |  switch (1) {
        |    case 1:
        |    case 2:
        |      a = 1;
        |      break;
        |  };
        |}""".stripMargin)
  }

  test("foreach") {

    assert(js(Array(1, 2) foreach { i => print(i) }).asString ===
      """for (var iColl = [1, 2], iIdx = 0, i = iColl[iIdx]; iIdx < iColl.length; i = iColl[++iIdx]) print(i)""")

    assert(js(Array[Int](1, 2) foreach { i => print(i) }).asString ===
      """for (var iColl = [1, 2], iIdx = 0, i = iColl[iIdx]; iIdx < iColl.length; i = iColl[++iIdx]) print(i)""")

    assert(js(Array("1", "2") foreach { i => print(i) }).asString ===
      """for (var iColl = ["1", "2"], iIdx = 0, i = iColl[iIdx]; iIdx < iColl.length; i = iColl[++iIdx]) print(i)""")

    assert(js(Seq(1, 2) foreach { i => print(i) }).asString ===
      """for (var iColl = [1, 2], iIdx = 0, i = iColl[iIdx]; iIdx < iColl.length; i = iColl[++iIdx]) print(i)""")

    assert(js(List(1, 2) foreach { i => print(i) }).asString ===
      """for (var iColl = [1, 2], iIdx = 0, i = iColl[iIdx]; iIdx < iColl.length; i = iColl[++iIdx]) print(i)""")

    assert(js(List("1", "2") foreach { i => print(i) }).asString ===
      """for (var iColl = ["1", "2"], iIdx = 0, i = iColl[iIdx]; iIdx < iColl.length; i = iColl[++iIdx]) print(i)""")

    assert(js(List[java.lang.String]("1", "2") foreach { i => print(i) }).asString ===
      """for (var iColl = ["1", "2"], iIdx = 0, i = iColl[iIdx]; iIdx < iColl.length; i = iColl[++iIdx]) print(i)""")

    val ast = js {
      val a = Array(1, 2)
      val b = Seq(1, 2)
      val c = List(1, 2)
      for (i <- a) print(i)
      for (i <- b) print(i)
      for (i <- c) print(i)
    }
    assert(ast.asString ===
      """{
        |  var a = [1, 2];
        |  var b = [1, 2];
        |  var c = [1, 2];
        |  for (var iColl = a, iIdx = 0, i = iColl[iIdx]; iIdx < iColl.length; i = iColl[++iIdx]) print(i);
        |  for (var iColl = b, iIdx = 0, i = iColl[iIdx]; iIdx < iColl.length; i = iColl[++iIdx]) print(i);
        |  for (var iColl = c, iIdx = 0, i = iColl[iIdx]; iIdx < iColl.length; i = iColl[++iIdx]) print(i);
        |}""".stripMargin)
  }

  test("callback func") {
    case class User(name: String)
    case class Project(title: String, users: Seq[User])

    assert(js({ (p: Project) =>
      if (p.title == "title")
        for (u <- p.users) print(u.name)
    }).asString === """function (p) {
            |    if (p.title == "title") for (var uColl = p.users, uIdx = 0, u = uColl[uIdx]; uIdx < uColl.length; u = uColl[++uIdx]) print(u.name);
            |  }""".stripMargin)
  }
}
