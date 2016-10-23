package org.jscala

import scala.reflect.macros.blackbox.Context
import scala.collection.generic.{MapFactory, SeqFactory}
import scala.reflect.macros.blackbox

/**
 * Author: Alexander Nemish
 * Date: 10/25/13
 * Time: 10:50 PM
 */
trait JsBasis[C <: blackbox.Context] extends MacroHelpers[C] {
  import c.universe._

  protected val unaryOps = Seq("+", "-", "!")
  protected val encodedUnaryOpsMap = unaryOps.map(op => TermName(s"unary_$op").encodedName -> op).toMap
  protected val binOps = Seq("*", "/", "%",  "+", "-", "<<", ">>", ">>>",
    "<", ">", "<=", ">=",
    "==", "!=", "&", "|", "^", "&&", "||")
  protected val encodedBinOpsMap = binOps.map(op => TermName(op).encodedName -> op).toMap

  protected lazy val jsString: PFT[String] = {
    case Literal(Constant(value: Char))  => value.toString
    case Literal(Constant(value: String))  => value
  }
  protected lazy val jsStringLit: ToExpr[JsString] = jsString.andThen(s => q"org.jscala.JsString($s)")

  protected lazy val jsNumLit: ToExpr[JsNum] = {
    case Literal(Constant(value: Byte))  => q"org.jscala.JsNum($value, isFloat = false)"
    case Literal(Constant(value: Short))  => q"org.jscala.JsNum($value, isFloat = false)"
    case Literal(Constant(value: Int))  => q"org.jscala.JsNum($value, isFloat = false)"
    case Literal(Constant(value: Long))  => q"org.jscala.JsNum($value, isFloat = false)"
    case Literal(Constant(value: Float))  => q"org.jscala.JsNum($value, isFloat = true)"
    case Literal(Constant(value: Double))  => q"org.jscala.JsNum($value, isFloat = true)"
  }
  protected lazy val jsBoolLit: ToExpr[JsBool] = {
    case Literal(Constant(value: Boolean))  => q"org.jscala.JsBool($value)"
  }
  protected object jsUnitLit extends PartialFunction[Tree, Tree] {
    def apply(v1: Tree) = q"org.jscala.JsUnit"
    def isDefinedAt(x: Tree) = isUnit(x)
  }
  protected object jsNullLit extends PartialFunction[Tree, Tree] {
    def apply(v1: Tree) = q"org.jscala.JsNull"
    def isDefinedAt(x: Tree) = isNull(x)
  }

  protected val jsLit: ToExpr[JsLit] = {
    jsStringLit orElse jsNumLit orElse jsBoolLit orElse jsNullLit orElse jsUnitLit
  }

  protected lazy val jsThis: ToExpr[JsIdent] = {
    case This(name) => q"""org.jscala.JsIdent("this")"""
  }

  protected lazy val jsIdent: ToExpr[JsIdent] = {
    case Ident(name) =>q"org.jscala.JsIdent(${name.decodedName.toString})"
  }

  protected lazy val jsJStringExpr: ToExpr[JsExpr] = {
    case Apply(Select(New(Select(Select(Ident(Name("org")), Name("jscala")), Name("JString"))), _), List(Literal(Constant(str: String)))) =>
      q"org.jscala.JsString($str)"
  }
}
