package org.jscalaexample

import org.scalatest.FunSuite
import org.jscala.typescript._

/**
 * @author Alexander Nemish
 */
class TypescriptedTest extends FunSuite {

  test("Jquery & Angular & Backbone parsing") {
    @Typescripted(file="jscala-examples/typescripts/jquery-ori.d.ts")
    object JQuery {
    }

    @Typescripted(file="jscala-examples/typescripts/angular-1.0.d.ts")
    object Angular {
      import JQuery._
    }

    @Typescripted(file="jscala-examples/typescripts/backbone.d.ts")
    object Backbone {
      import JQuery._
    }

    assert(JQuery.jquery != null)
    assert(Angular.ng != null)
    assert(Backbone.Backbone != null)
  }

}
