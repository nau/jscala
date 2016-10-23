package org.jscalaexample

import jdk.nashorn.api.scripting.ScriptObjectMirror
import org.jscala._
import org.scalatest.{FunSuite, Matchers}
import org.scalatest.prop.{Checkers, PropertyChecks}

import collection.JavaConverters._

class AesTest extends FunSuite with Matchers with PropertyChecks with Checkers {
  test("test") {
    val key = Array(1, 1, 1, 1)
    forAll((d: Array[Int]) => whenever(d.size >= 4) {
      val data = d.take(4)
      val aes = new Aes(key)
      val encrypted = aes.crypt(data, false)
      val decrypted = aes.crypt(encrypted, true)
      val main = javascript {
        val d = inject(data)
        val k = inject(key)
        val aes = new Aes(k)
        val encrypted = aes.crypt(d, false)
        aes.crypt(encrypted, true)
      }
      val js = Aes.jscala.javascript ++ main
      val jsDecrypted = js.eval().asInstanceOf[ScriptObjectMirror].values().asScala.toList
      data should be(decrypted)
      data.toList should be(jsDecrypted)
    })
  }
}
