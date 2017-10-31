package com.something.unreleated

import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal

object AnotherModule {

  @js.native
  trait CustomClass {
    def print(): Unit = js.native
  }

  @js.native
  class MyCustomJsClass(str: String) extends CustomClass {
    override def print(): Unit = js.native
  }

  @js.native
  @JSGlobal
  object SomethingGlobal extends js.Object {
    def init(): Unit = js.native

    val count: Int = js.native

    def use(customClass: CustomClass): Unit = js.native

    def close(): Unit = js.native
  }

}
