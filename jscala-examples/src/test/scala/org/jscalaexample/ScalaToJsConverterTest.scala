package org.jscalaexample

import org.scalatest.FunSuite
import org.jscala._
import org.jscala.{javascript=>js}
import scala.collection.mutable.ArrayBuffer

class ScalaToJsConverterTest extends FunSuite {
  test("Literals") {
    assert(js(1) === 1.toJs)
    assert(js(-5.1) === (-5.1).toJs)
    assert(js('c') === "c".toJs)
    assert(js("str") === "str".toJs)
    assert(js(true) === true.toJs)
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
    val f = 2.0
    val c = true
    val d = false
    var s = 0
    val ja = JsIdent("a")
    val jb = JsIdent("b")
    val jc = JsIdent("c")
    val jd = JsIdent("d")
    val jf = JsIdent("f")
    assert(js(a * b) === JsBinOp("*", ja, jb))
    assert(js(b / f) === JsBinOp("/", jb, jf))
    assert(js(a / b) === JsBinOp("|", JsBinOp("/", ja, jb), 0.toJs))
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
    val lhs = JsBinOp("!=", JsBinOp("/", JsBinOp("+", ja, JsBinOp("*", jb, ja)), jf), jb)
    val expected = JsBinOp("&&", lhs, JsBinOp("==", jc, jb))
    assert(js(((a + b * a) / f != b) && c == b) === expected)
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
    val ast = js {
      val obj = new {
        val field = 1
        def func1(i: Int) = field
        def func2(i: Int) = "string"
      }
    }
    val fields = List("field" -> 1.toJs, "func1" -> JsAnonFunDecl(List("i"), JsReturn(JsSelect(JsIdent("this"), "field")).block), "func2" -> JsAnonFunDecl(List("i"), JsReturn("string".toJs).block))
    assert(ast === varDef("obj", JsAnonObjDecl(fields)).block)
    assert(js(obj.func2(1)) === JsCall(JsSelect(JsIdent("obj"), "func2"), List(1.toJs)))
    val lambda = JsAnonFunDecl(List("i"), JsReturn(JsCall(JsSelect(JsIdent("i"), "toString"), Nil)).block)
    assert(js(func3(i => i.toString)) === JsCall(JsIdent("func3"), List(lambda)))

    val ifExprAst = js {
      var a = if (inject(local) > 1) 2 else 3
      val b = if (inject(local) > 1) {
        a += 1
        a
      } else {
        a += 2
        a
      }
      a + b
    }
    assert(ifExprAst.eval() === 10)
    val matchExprAst = js {
      val a = inject(local) match {
        case 1 | 2 => "one or two"
        case 3 => "three"
        case _ => "other"
      }
      a
    }
    assert(matchExprAst.eval() === "one or two")
    val ternaryAst = js {
      val a = if (Math.PI > 3) 3 else 4
      def f(i: Int) = i
      f(if (Math.PI > 3) 3 else 4) + a
    }
    assert(ternaryAst.eval() === 6)
  }

  test("Closures") {
    val ast = js {
      class A {
      def foo(f: Int => String) = f(1)
      foo(_.toString)
      def bar(i: Int) = i.toString
      foo(bar)
      }
    }
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
    assert(js { val a = 0 } === varDef("a", 0.toJs).block)
    assert(js { val a = b + 2 } === varDef("a", JsBinOp("+", JsIdent("b"), 2.toJs)).block)
    assert(js { if (a > 0) b } === JsIf(JsBinOp(">", ja, 0.toJs), jb, None))
    val stmt1 = JsCall(JsSelect(JsIdent("console"), "log"), List("".toJs))
    assert(js { if (a > 0) console.log("") else a } === JsIf(JsBinOp(">", ja, 0.toJs), stmt1, Some(ja)))
    val whileBody = JsCall(JsSelect(JsIdent("Math"), "random"), List())
    assert(js { while (a > 0) Math.random() } === JsWhile(JsBinOp(">", ja, 0.toJs), whileBody))
    val body = JsBlock(List(JsBinOp("=", jss, JsBinOp("+", jss, ja)), JsBinOp("=", ja, JsBinOp("+", ja, 1.toJs))))
    val expected = JsWhile(JsBinOp(">", ja, 0.toJs), body)
    assert(js {
      while (a > 0) {
        s = s + a
        a = a + 1
      } } === expected)

    assert(js { def func1() {} } === JsFunDecl("func1", Nil, JsBlock(Nil)).block)
    assert(js { def func2 = 5 } === JsFunDecl("func2", Nil, JsBlock(List(JsReturn(5.toJs)))).block)
    assert(js { def func3() = 5 } === JsFunDecl("func3", Nil, JsBlock(List(JsReturn(5.toJs)))).block)
    assert(js { def func4(a: String) = 5 } === JsFunDecl("func4", List("a"), JsBlock(List(JsReturn(5.toJs)))).block)
    val ifElse = Some(JsBlock(List(varDef("c", JsBinOp("*", jb, 2.0.toJs)), JsUnOp("-", jc))))
    val jsIf = JsIf(JsBinOp(">", ja, 2.toJs), JsBinOp("*", JsBinOp("+", ja, jb), 2.toJs), ifElse)
    val bodyfunc5 = JsBlock(List(varDef("b", 5.0.toJs), jsIf))
    val expectedFunc5 = JsFunDecl("func5", List("a"), bodyfunc5).block
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

    val els = Some(JsCall(JsSelect(JsIdent("console"), "log"), List("a".toJs)))
    val func6Body = JsIf(JsBinOp(">", JsSelect(ja, "length"), 2.toJs), JsReturn(JsUnit), els).block
    assert(js { def func6(a: String) {
      if (a.length > 2) return else {
        console.log("a")
      }
    } } === JsFunDecl("func6",List("a"),func6Body).block)
  }

  test("Val access") {
    assert(javascript("String".jstr.length) === JsSelect(JsString("String"), "length"))
  }

  test("for to/until") {
    val ast = js {
      val a = Array(1, 2)
      for (i <- a(0) until a.length) print(a(i))
      for (i <- 0 to a.length) print(a(i))
    }
    val stmt = JsCall(JsIdent("print"), List(JsAccess(JsIdent("a"), JsIdent("i"))))
    val jsForUntil = JsFor(List(varDef("i", JsAccess(JsIdent("a"), 0.toJs))), JsBinOp("<", JsIdent("i"), JsSelect(JsIdent("a"), "length")), List(JsUnOp("++", JsIdent("i"))), stmt)
    val jsForTo = JsFor(List(varDef("i", 0.toJs)), JsBinOp("<=", JsIdent("i"), JsSelect(JsIdent("a"), "length")), List(JsUnOp("++", JsIdent("i"))), stmt)
    assert(ast === JsBlock(List(varDef("a", JsArray(List(1.toJs, 2.toJs))), jsForUntil, jsForTo)))
  }

  test("for in") {
    val js = javascript {
      val a = Array(1, 2)
      val b = Seq("1", "2")
      val c = Map("1" -> 1, "2" -> 2)
      val d = new { val a = 1 }
      forIn(a) { i => print(i) }
      forIn(b) { i => print(i) }
      forIn(c) { i => print(i) }
      forIn(d) { i => print(i) }
    }
    /// FIXME: use === instead when scalatest for 2.11 is ready
    assert(js === JsBlock(List(
      JsVarDef(List(("a", JsArray(List(1.toJs, 2.toJs))))),
      JsVarDef(List(("b", JsArray(List("1".toJs, "2".toJs))))),
      JsVarDef(List(("c", JsAnonObjDecl(List("1" -> 1.toJs, "2" -> 2.toJs))))),
      JsVarDef(List(("d", JsAnonObjDecl(List("a" -> 1.toJs))))),
      JsForIn(JsIdent("i"), JsIdent("a"), JsCall(JsIdent("print"), List(JsIdent("i")))),
      JsForIn(JsIdent("i"), JsIdent("b"), JsCall(JsIdent("print"), List(JsIdent("i")))),
      JsForIn(JsIdent("i"), JsIdent("c"), JsCall(JsIdent("print"), List(JsIdent("i")))),
      JsForIn(JsIdent("i"), JsIdent("d"), JsCall(JsIdent("print"), List(JsIdent("i"))))
    )))
  }

  test("String operations") {
    val ast = js {
      val a = new JString("str")
      a.replace("s", "a").slice(1, 2)
    }
    val call = JsCall(JsSelect(JsCall(JsSelect(JsIdent("a"), "replace"), List("s".toJs, "a".toJs)), "slice"), List(1.toJs, 2.toJs))
    assert(ast === JsBlock(List(varDef("a","str".toJs), call)))
    val ast1 = js("String".length)
    assert(ast1 === JsSelect(JsString("String"), "length"))
  }
  
  test("String interpolations") {
    val ast = js {
      val a = 1
      s"string"
      s"$a test"
      s"a = $a and string is ${a.toString}"
    }
    assert(ast === varDef("a", 1.toJs) ++
      "string".toJs ++
      JsBinOp("+", JsBinOp("+", "".toJs, JsIdent("a")), " test".toJs) ++
      JsBinOp("+", JsBinOp("+", JsBinOp("+", JsBinOp("+", "a = ".toJs, JsIdent("a")), " and string is ".toJs), JsCall(JsSelect(JsIdent("a"), "toString"), Nil)), "".toJs))
  }

  test("Arrays") {
    assert(js(Tuple1(1)) === JsArray(List(1.toJs)))
    assert(js((1, 2)) === JsArray(List(1.toJs, 2.toJs)))
    assert(js(("1", "2")) === JsArray(List("1".toJs, "2".toJs)))
    assert(js((1, 2, 3)) === JsArray(List(1.toJs, 2.toJs, 3.toJs)))
    assert(js((1, 2, 3, 4)) === JsArray(List(1.toJs, 2.toJs, 3.toJs, 4.toJs)))

    assert(js(Array(1, 2)) === JsArray(List(1.toJs, 2.toJs)))
    val arint = js {
      val a = new Array[Int](25)
      a(0) = 1
      a(0)
    }
    assert(arint.eval() === 1)
    assert(js(Array("1", "2")) === JsArray(List("1".toJs, "2".toJs)))
    val call = JsCall(JsSelect(JsIdent("console"), "log"), List(JsIdent("i")))
    val init = List(JsVarDef(List(("iColl", JsIdent("a")), ("iIdx", 0.toJs), ("i", JsAccess(JsIdent("iColl"), JsIdent("iIdx"))))))
    val check = JsBinOp("<", JsIdent("iIdx"), JsSelect(JsIdent("iColl"), "length"))
    val update = List(JsBinOp("=", JsIdent("i"), JsAccess(JsIdent("iColl"), JsUnOp("++", JsIdent("iIdx")))))
    assert(js{
      val a = Array("1", "2")
      for (i <- a) console.log(i)

    } === JsBlock(List(varDef("a", JsArray(List("1".toJs, "2".toJs))), JsFor(init, check, update, call))))
    val call1 = JsCall(JsSelect(JsIdent("a"), "pop"), Nil)
    assert(js {
      val a = JArray("1", "2")
      a.pop()
    } === JsBlock(List(varDef("a", JsArray(List("1".toJs, "2".toJs))), call1)))

    val assign = JsBinOp("=", JsAccess(JsIdent("a"), 0.toJs), JsAccess(JsIdent("a"), 1.toJs))
    val init1 = List(JsVarDef(List(("iIdx", 0.toJs), ("i", JsAccess(JsIdent("a"), JsIdent("iIdx"))))))
    val check1 = JsBinOp("<", JsIdent("iIdx"), JsSelect(JsIdent("a"), "length"))
    val update1 = List(JsBinOp("=", JsIdent("i"), JsAccess(JsIdent("a"), JsUnOp("++", JsIdent("iIdx")))))
    val forStmt = JsFor(init1, check1, update1, JsCall(JsIdent("print"), List(JsIdent("i"))))
    assert(js {
      val a = JArray(1, 2)
      for (i <- a) print(i)
      a(0) = a(1)
      a.pop()
    } === JsBlock(List(varDef("a", JsArray(List(1.toJs, 2.toJs))), forStmt, assign, call1)))

    val ast = js {
      val a = Seq("1", "2")
      val b = ArrayBuffer(1, 2)
      parseInt(a(0)) + b(1)
    }
    assert(ast.eval() === 3)
  }

  test("Math") {
    val ast = js(Math.abs(-1))
    assert(ast === JsCall(JsSelect(JsIdent("Math"), "abs"), List((-1.0).toJs)))
  }

  test("Global functions") {
    val ast = js(escape("asdf"))
    assert(ast === JsCall(JsIdent("escape"), List("asdf".toJs)))
    val ast1 = js(typeof("asdf"))
    assert(ast1 === JsCall(JsIdent("typeof"), List("asdf".toJs)))
    val ast2 = js {
      val a = include("[1, 2]").asInstanceOf[JArray[Int]]
      if (a.length > 1) {
        include("console.log(a[1])")
      }
    }
    val jsIf = JsIf(JsBinOp(">", JsSelect(JsIdent("a"), "length"), 1.toJs), JsRaw("console.log(a[1])"), None)
    assert(ast2 === JsBlock(List(varDef("a", JsRaw("[1, 2]")), jsIf)))
    object L {
      class A
      def f(x: Int): Int = ???
    }
    val ast3 = js { L.f(3) }
    assert(ast3 === JsCall(JsIdent("f"), List(3.toJs)))
    val ast4 = js { new L.A() }
    assert(ast4 === JsNew(JsCall(JsIdent("A"), Nil)))
  }

  test("RegExp") {
    val ast = js {
      val a = new RegExp("d.*", "g")
      a.exec("asdf").toString()
    }
    val call = JsCall(JsSelect(JsCall(JsSelect(JsIdent("a"), "exec"), List("asdf".toJs)), "toString"), Nil)
    assert(ast === JsBlock(List(varDef("a",JsNew(JsCall(JsIdent("RegExp"),List("d.*".toJs, "g".toJs)))), call)))
  }

  test("Date") {
    val ast = js {
      val a = new Date()
      a.getDay().toString()
    }
    val call = JsCall(JsSelect(JsCall(JsSelect(JsIdent("a"), "getDay"), Nil), "toString"), Nil)
    assert(ast === JsBlock(List(varDef("a", JsNew(JsCall(JsIdent("Date"), Nil))), call)))
  }

  test("Maps") {
    import collection.mutable
    // Immutable Map
    val ast = js {
      val a = Map("field" -> JArray(1, 2), ("field2", JArray(1)), "field3" -> JArray[Int]())
      a("field").pop().toString()
    }
    val call1 = JsCall(JsSelect(JsCall(JsSelect(JsAccess(JsIdent("a"), "field".toJs), "pop"), Nil), "toString"), Nil)
    val fields = List("field" -> JsArray(List(1.toJs, 2.toJs)), "field2" -> JsArray(List(1.toJs)), "field3" -> JsArray(Nil))
    assert(ast === JsBlock(List(varDef("a", JsAnonObjDecl(fields)), call1)))

    // Mutable Map
    val ast1 = js {
      val a = mutable.Map("field" -> JArray(1, 2), ("field2", JArray(1)), "field3" → JArray[Int]())
      a("field") = a("field2")
    }
    val stmt = JsBinOp("=", JsAccess(JsIdent("a"), "field".toJs), JsAccess(JsIdent("a"), "field2".toJs))
    assert(ast1 === JsBlock(List(varDef("a", JsAnonObjDecl(fields)), stmt)))
  }

  test("Switch declaration") {
    val ast = javascript {
      val a: Any = "2"
      a match {
        case 1 | 2 => "1"
        case "2" => "2"
        case true => "true"
        case _ => "3"
      }
    }
    assert(ast.eval() === "2")
  }

  test("Try/catch/finally") {
    val ast = javascript {
      try { 1 } catch {
        case  e: Exception => 2
      }
    }
    assert(ast.eval() === 1.0)
    val ast1 = javascript { try { 1 } finally { print(2) }}
    assert(ast1 === JsTry(1.toJs, None, Some(JsCall(JsIdent("print"), List(2.toJs)))))
    val ast2 = javascript {
      try { 1 } catch {
        case  e: Exception => 2
      } finally { print(3) }
    }
    assert(ast2 === JsTry(1.toJs, Some(JsCatch(JsIdent("e"), 2.toJs)), Some(JsCall(JsIdent("print"), List(3.toJs)))))
  }

  test("Throw") {
    val ast = javascript(throw new IndexOutOfBoundsException())
    /// FIXME: use === instead when scalatest for 2.11 is ready
    assert(ast == JsThrow(JsNew(JsCall(JsSelect(JsIdent("package"), "IndexOutOfBoundsException"), Nil))))
  }

  test("Object declaration") {
    val ast = javascript {
      class A(val arg1: String, var arg2: Int, arg3: Array[String]) {
        val field = 1
        var count = arg2
        count = 5
        def func1(i: Int) = field
        def func2(i: Int) = arg3(i)
        def func3() = arg3
        def func4() = func3()(0)
        def func5() = count
      }
      val a = new A("a", 1, Array("111", "222"))
      a.arg1 + a.func2(a.arg2) + a.field + a.func4() + a.func5()
    }
    assert(ast.eval() === "a22211115")
  }

  test("Lazy") {
    val x = 15
    val y = "hehe"
    def f() = "1"
    case class My(a: String)
    implicit def zzz: JsSerializer[My] = new JsSerializer[My] { def apply(a: My) = JsString(a.a) }
    val z = My("my")
    val ls = Seq(1, 2, 3).map(_.toJs)
    def genJsAst() = "gen".toJs
    val ast = javascript {
      val a = inject(x)
      val b = inject(y)
      val c = inject(z)
      val d = inject(f _)
      val e = inject(ls)
      val gen = genJsAst()
      a.as[String] + b + c + d + e.toString() + gen
    }
    assert(ast.eval() === "15hehemy11,2,3gen")
  }

  test("() => JsAst injection") {
    var t = 1
    def handle: Int => Unit = (i: Int) => { t = i }
    def ajax(f: Int => Unit) = {
      f(2)
      javascript { (y: Int) => y.toString() }
    }

    val js = javascript {
      val x = 10
      val s = ajax(handle)(x)
      "JavaScript " + s
    }
    js.asString
    assert(t === 2)
    assert(js.eval() === "JavaScript 10")
  }

  test("Implicit conversions") {
    val ast = javascript {
      val a: JString = "asdf"
      val b: JArray[Int] = Array(1, 2)
      val c: JArray[String] = ArrayBuffer("1", "2")
      def f(s: String, b: Array[Int], c: Array[String]) = s
      f(a, b, c)
    }
    assert(ast.eval() === "asdf" )
  }

  test("JsDynamic") {
    val $ = new JsDynamic {}
    class XmlHttpRequest(s: String) extends JsDynamic
    val ast = javascript {
      val a = new XmlHttpRequest("request")
      a.foo("reply")
      $("button", this).click(() => $("p").css("color", "red"))
      $.field = $.select
    }
    val request = varDef("a",JsNew(JsCall(JsIdent("XmlHttpRequest"),List("request".toJs))))
    val foo = JsCall(JsSelect(JsIdent("a"),"foo"),List("reply".toJs))
    val returnExpr = JsCall(JsSelect(JsCall(JsSelect(JsIdent("$"), "apply"), List("p".toJs)), "css"), List("color".toJs, "red".toJs))
    val anonFunDecl = JsAnonFunDecl(List(), JsReturn(returnExpr).block)
    val call = JsCall(JsSelect(JsCall(JsSelect(JsIdent("$"), "apply"), List("button".toJs, JsIdent("this"))), "click"), List(anonFunDecl))
    val update = JsBinOp("=", JsSelect(JsIdent("$"), "field"), JsSelect(JsIdent("$"), "select"))
    assert(ast === JsBlock(List(request, foo, call, update)))
  }

  test("Traits") {
    val ast = javascript {
      trait A {
        def f1() = 1
        def abstr(i: Int): String
        val self = this
      }
      trait B
      trait C extends B
      class D(p: String) extends A with C {
        def abstr(i: Int) = i.toString()
      }
      def bar(a: A) = a.abstr(a.self.f1())
      val a = new D("Test")
      bar(a)
    }
//    println(ast.asString)
    assert(ast.eval() === "1")
  }
}
