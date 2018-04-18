package org.jscala.tests

import org.jscala.javascript
import org.specs2.mutable._

import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal

class JsSpecs extends SpecificationWithJUnit {

  import com.something.unreleated.AnotherModule._

  "javascript" should {
    "detect global js objects" in {
      val js = javascript {
        SomethingGlobal.init()
      }
      js.asString === "SomethingGlobal.init()"
    }

    "use values in global objects" in {
      val js = javascript {
        SomethingGlobal.count * 2
      }
      js.asString === "SomethingGlobal.count * 2"
    }

    "use scala.js facades in parameters on global functions" in {
      val js = javascript {
        SomethingGlobal.use(new MyCustomJsClass("hello"))
      }
      js.asString === """SomethingGlobal.use(new com.something.unreleated.AnotherModule.MyCustomJsClass("hello"))"""
    }

    "use global js objects in statements" in {
      val js = javascript {
        SomethingGlobal.init()
        SomethingGlobal.use(new MyCustomJsClass("hello"))
        SomethingGlobal.close()
      }
      js.asString ===
        """{
          |  SomethingGlobal.init();
          |  SomethingGlobal.use(new com.something.unreleated.AnotherModule.MyCustomJsClass("hello"));
          |  SomethingGlobal.close();
          |}""".stripMargin
    }
  }

}
