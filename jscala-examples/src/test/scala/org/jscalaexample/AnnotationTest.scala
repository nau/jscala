package org.jscalaexample

import org.scalatest.FunSuite
import org.jscala._

/**
 * @author Alexander Nemish
 */
class AnnotationTest extends FunSuite {

  test("Class") {
    @JavaScript class Aes(val key: Array[Int]) {
      val encTable = Array(new Array[Int](256), new Array[Int](256), new Array[Int](256), new Array[Int](256), new Array[Int](256))
      def f1() = 15
    }
    object Aes {
      def test = 3
    }
    val a = new Aes(Array(1, 1, 1, 1)) 
//    println(Aes.javaScript.asString)
  }

}
