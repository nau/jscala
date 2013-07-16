package com.github.nau.jscala

import org.scalatest.FunSuite
import com.github.nau.jscala.{javascript=>js}

class ScalaToJsConverterTest extends FunSuite {
  test("Literals") {
    assert(js(1) === JsNum(1.0, isFloat = false))
    assert(js(-5.1) === JsNum(-5.1, isFloat = true))
    assert(js('c') === JsString("c"))
    assert(js("str") === JsString("str"))
    assert(js(true) === JsBool(true))
    assert(js(()) === JsUnit)
  }

  test("Unary operators") {
    val a = 0
    val b = false
    assert(js(+a) === JsUnOp("+", JsIdent("a")))
    assert(js(-a) === JsUnOp("-", JsIdent("a")))
    assert(js(!b) === JsUnOp("!", JsIdent("b")))
  }

  test("Binary operators") {
    val a = 0
    val b = 1
    val c = true
    val d = false
    var s = 0
    val ja = JsIdent("a")
    val jb = JsIdent("b")
    val jc = JsIdent("c")
    val jd = JsIdent("d")
    assert(js(a * b) === JsBinOp("*", ja, jb))
    assert(js(a / b) === JsBinOp("/", ja, jb))
    assert(js(a % b) === JsBinOp("%", ja, jb))
    assert(js(a + b) === JsBinOp("+", ja, jb))
    assert(js(a - b) === JsBinOp("-", ja, jb))
    assert(js(a << b) === JsBinOp("<<", ja, jb))
    assert(js(a >> b) === JsBinOp(">>", ja, jb))
    assert(js(a >>> b) === JsBinOp(">>>", ja, jb))
    assert(js(a < b) === JsBinOp("<", ja, jb))
    assert(js(a > b) === JsBinOp(">", ja, jb))
    assert(js(a <= b) === JsBinOp("<=", ja, jb))
    assert(js(a >= b) === JsBinOp(">=", ja, jb))
    assert(js(a == b) === JsBinOp("==", ja, jb))
    assert(js(a != b) === JsBinOp("!=", ja, jb))
    assert(js(a & b) === JsBinOp("&", ja, jb))
    assert(js(a | b) === JsBinOp("|", ja, jb))
    assert(js(c && d) === JsBinOp("&&", jc, jd))
    assert(js(c || d) === JsBinOp("||", jc, jd))
    assert(js{s = a + b} === JsBinOp("=", JsIdent("s"), JsBinOp("+", ja, jb)))
    val lhs = JsBinOp("!=", JsBinOp("/", JsBinOp("+", ja, JsBinOp("*", jb, ja)), ja), jb)
    val expected = JsBinOp("&&", lhs, JsBinOp("==", jc, jb))
    assert(js(((a + b * a) / a != b) && c == b) === expected)
  }

  test("Simple expressions") {
    val local = 1
    def func1() = 1
    val obj = new {
      val field = 1
      def func2(i: Int) = "string"
    }
    def func3(f: Int => String) = f(1)
    assert(js(local) === JsIdent("local"))
    assert(js(func1()) === JsCall(JsIdent("func1"), Nil))
    assert(js(obj.field) === JsSelect(JsIdent("obj"), "field"))
    assert(js(obj.func2(1)) === JsCall(JsSelect(JsIdent("obj"), "func2"), List(JsNum(1, false))))
    val lambda = JsAnonFunDecl(List("i"), JsBlock(List(JsReturn(JsCall(JsSelect(JsIdent("i"), "toString"), Nil)))))
    assert(js(func3(i => i.toString)) === JsCall(JsIdent("func3"), List(lambda)))
  }

  test("Simple statements") {
    var a = 0
    val b = 1
    val c = true
    val d = false
    var s = 0
    val ja = JsIdent("a")
    val jb = JsIdent("b")
    val jc = JsIdent("c")
    val jd = JsIdent("d")
    val jss = JsIdent("s")
    assert(js { val a = 0 } === JsBlock(List(JsVarDef("a", JsNum(0, false)), JsUnit)))
    assert(js { val a = b + 2 } === JsBlock(List(JsVarDef("a", JsBinOp("+", JsIdent("b"), JsNum(2, false))), JsUnit)))
    assert(js { if (a > 0) b } === JsIf(JsBinOp(">", ja, JsNum(0, false)), JsExprStmt(jb), None))
    assert(js { if (a > 0) b else a } === JsIf(JsBinOp(">", ja, JsNum(0, false)), JsExprStmt(jb), Some(JsExprStmt(ja))))
    assert(js { while (a > 0) b } === JsWhile(JsBinOp(">", ja, JsNum(0, false)), JsExprStmt(jb)))
    val body = JsBlock(List(JsExprStmt(JsBinOp("=", jss, JsBinOp("+", jss, ja))), JsExprStmt(JsBinOp("=", ja, JsBinOp("+", ja, JsNum(1.0, false))))))
    val expected = JsWhile(JsBinOp(">", ja, JsNum(0, false)), body)
    assert(js {
      while (a > 0) {
        s = s + a
        a = a + 1
      } } === expected)

    assert(js { def func1() {} } === JsBlock(List(JsFunDecl("func1", Nil, JsBlock(Nil)), JsUnit)))
    assert(js { def func2 = 5 } === JsBlock(List(JsFunDecl("func2", Nil, JsBlock(List(JsReturn(JsNum(5.0,false))))), JsUnit)))
    assert(js { def func3() = 5 } === JsBlock(List(JsFunDecl("func3", Nil, JsBlock(List(JsReturn(JsNum(5.0,false))))), JsUnit)))
    assert(js { def func4(a: String) = 5 } === JsBlock(List(JsFunDecl("func4", List("a"), JsBlock(List(JsReturn(JsNum(5.0,false))))), JsUnit)))
    val ifElse = Some(JsBlock(List(JsVarDef("c", JsBinOp("*", jb, JsNum(2.0, true))), JsExprStmt(JsUnOp("-", jc)))))
    val jsIf = JsIf(JsBinOp(">", ja, JsNum(2.0, false)), JsExprStmt(JsBinOp("*", JsBinOp("+", ja, jb), JsNum(2.0, false))), ifElse)
    val bodyfunc5 = JsBlock(List(JsVarDef("b", JsNum(5.0, true)), jsIf))
    val expectedFunc5 = JsBlock(List(JsFunDecl("func5", List("a"), bodyfunc5), JsUnit))
    assert(js {
      def func5(a: Int) = {
        val b = 5.0
        if (a > 2)
          (a + b) * 2
        else {
          val c = b * 2.0
          -c
        }
      }
    } === expectedFunc5)

    val els = Some(JsExprStmt(JsCall(JsIdent("println"),List(JsString("a")))))
    val func6Body = JsBlock(List(JsIf(JsBinOp(">", JsCall(JsSelect(ja, "length"), Nil), JsNum(2.0, false)), JsReturn(JsUnit), els)))
    assert(js { def func6(a: String) {
      if (a.length > 2) return else {
        println("a")
      }
    } } === JsBlock(List(JsFunDecl("func6",List("a"),func6Body), JsUnit)))
  }

  test("Arrays") {
    assert(js(Array(1, 2)) === JsArray(List(JsNum(1.0, false), JsNum(2.0, false))))
    assert(js(Array("1", "2")) === JsArray(List(JsString("1"), JsString("2"))))
    assert(js{
      val a = Array("1", "2")
      for (i <- a) println(i)
    } === JsBlock(List(JsVarDef("a", JsArray(List(JsString("1"), JsString("2")))), JsFor(JsIdent("a"), JsIdent("i"), JsExprStmt(JsCall(JsIdent("println"), List(JsIdent("i"))))))))
  }
}
