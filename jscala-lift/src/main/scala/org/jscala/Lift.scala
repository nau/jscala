package org.jscala

import language.experimental.macros
import scala.reflect.macros.Context
import net.liftweb.http.SHtml
import net.liftweb.http.js.{JsCmds, JE, JsExp}

object Lift {

  implicit def jsAst2JsExp(a: JsAst): JsExp = JE.JsRaw(a.asString)

  def liftAjax[A](expr: A)(callback: String => JsAst) = macro liftAjaxImpl[A]
  def liftAjaxImpl[A: c.WeakTypeTag](c: Context)(expr: c.Expr[A])(callback: c.Expr[String => JsAst]) = {
    import c.universe._
    val in = javascriptImpl(c)(expr)
    reify(SHtml.ajaxCall(in.splice, s => JsCmds.Run(callback.splice.apply(s).asString)))
  }
}
