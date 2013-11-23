package org.jscalaexample

import org.scalatest.FunSuite
import org.jscala._

/**
 * @author Alexander Nemish
 */

  @Javascript(debug = true, json = true) class Role(val name: String)
//  @Javascript class User(val name: String, val id: Int, val roles: Set[Role])


object Role {

}


class AnnotationTest extends FunSuite {

  /*test("Class") {
    @Javascript class Aes(val key: Array[Int]) {
      val encTable = Array(new Array[Int](256), new Array[Int](256), new Array[Int](256), new Array[Int](256), new Array[Int](256))
      def f1() = 15
    }
    object Aes {
      def test = 3
    }
    val a = new Aes(Array(1, 1, 1, 1)) 
//    println(Aes.javaScript.asString)
  }*/

  test("Json") {

//    val user = new User(null, 2, Set(new Role("admin"), new Role("user")))
//    val js = user.js.json
//    println(js.asString)
//    fromJson[User]("""{"name": "alex", "id": 123, "roles": [{"name": "user"}, {"name": "admin"}]}""")
//    val u1 = User.jscala.fromJson(json)
    val role = new Role("alex")
//    val js = role.js.json
//    println(js.asString)
//    println(fromJson[Role](js.asString))

//    assert(user === u1)
  }

}
