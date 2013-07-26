package org.jscala

import language.experimental.macros
import scala.reflect.macros.Context
import net.liftweb.http.SHtml
import net.liftweb.http.js.{JsCmd, JsCmds, JE, JsExp}
import net.liftweb.json.JsonAST._

object Lift {

  implicit def jsAst2JsExp(a: JsAst): JsExp = JE.JsRaw(a.asString)
  implicit def jValue2A(a: JsAst): JsExp = JE.JsRaw(a.asString)
  def jsonCall(expr: JsExp, f: JValue => JsCmd) = expr.toJsCmd + f(JInt(123)).toJsCmd

  def liftAjax[A](expr: A)(callback: A => JsAst) = macro liftAjaxImpl[A]
  def liftAjaxImpl[A: c.WeakTypeTag](c: Context)(expr: c.Expr[A])(callback: c.Expr[A => JsAst]) = {
    import c.universe._
    val in = javascriptImpl(c)(expr)
    val t = weakTypeOf[A]
    val s = c.Expr[JValue](Ident(newTermName("s")))
    val input = if (t weak_<:< typeOf[Long]) reify(s.splice.asInstanceOf[JInt].num.toLong.asInstanceOf[A])
    else if (t weak_<:< typeOf[Double]) reify(s.splice.asInstanceOf[JDouble].num.asInstanceOf[A])
    else if (t =:= typeOf[String]) reify(s.splice.asInstanceOf[net.liftweb.json.JsonAST.JString].s.asInstanceOf[A])
    else c.abort(c.enclosingPosition, s"Unsupported JValue $s")
    val cb = reify { (s: JValue) => JsCmds.Run(callback.splice.apply(input.splice).asString) }
    reify(jsonCall(jsAst2JsExp(in.splice), cb.splice))
    //    reify(SHtml.jsonCall(jsAst2JsExp(in.splice), cb.splice))
  }
}
