package org.jscalaexample

import org.scalatest.FunSuite
import org.jscala._

/**
 * @author Alexander Nemish
 */

@Javascript(debug = false, json = true) case class Role(name: String)
@Javascript case class User(name: String, id: Int, roles: Set[Role])


class AnnotationTest extends FunSuite {

  test("JSON Array") {
    val js = toJson(Array(1, 2))
    assert(js === JsArray(List(1.toJs, 2.toJs)))
    val from = fromJson[Array[Int]](js.asString)
    assert(from === Array(1, 2))
  }

  test("JSON Map") {
    val js = toJson(Map("1" -> 2, "3" -> 4))
    assert(js === JsAnonObjDecl(List("1" -> 2.toJs, "3" -> 4.toJs)))
    val from = fromJson[Map[String, Int]](js.asString)
    assert(from === Map("1" -> 2, "3" -> 4))
  }

  test("JSON Object") {
    class Test(val a: String, val b: Array[Map[String, Double]])
    val test = new Test(null, Array(Map("1" -> 1.0), Map("two" -> 2.0)))
    val json = toJson(test)
    val test1 = fromJson[Test](json.asString)
    assert(test.a === test1.a)
    assert(test.b(0)("1") === test1.b(0)("1"))
    assert(test.b(1)("two") === test1.b(1)("two"))
  }

  test("JSON Object Hierarchy") {
    val user = new User("nau", 2, Set(new Role("admin"), new Role("user"), new Role(null)))
    val js = user.js.json
    val u1 = User.jscala.fromJson(js.asString)
    assert (u1 === user)
  }
}
