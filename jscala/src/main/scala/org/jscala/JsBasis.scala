package org.jscala

import scala.reflect.macros.Context
import scala.collection.generic.{MapFactory, SeqFactory}

/**
 * Author: Alexander Nemish
 * Date: 10/25/13
 * Time: 10:50 PM
 */
trait JsBasis[C <: Context] extends MacroHelpers[C] {
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
  protected lazy val jsStringLit: ToExpr[JsString] = jsString.andThen(s => q"JsString($s)")

  protected lazy val jsNumLit: ToExpr[JsNum] = {
    case Literal(Constant(value: Byte))  => q"JsNum($value, isFloat = false)"
    case Literal(Constant(value: Short))  => q"JsNum($value, isFloat = false)"
    case Literal(Constant(value: Int))  => q"JsNum($value, isFloat = false)"
    case Literal(Constant(value: Long))  => q"JsNum($value, isFloat = false)"
    case Literal(Constant(value: Float))  => q"JsNum($value, isFloat = true)"
    case Literal(Constant(value: Double))  => q"JsNum($value, isFloat = true)"
  }
  protected lazy val jsBoolLit: ToExpr[JsBool] = {
    case Literal(Constant(value: Boolean))  => q"JsBool($value)"
  }
  protected object jsUnitLit extends PartialFunction[Tree, Tree] {
    def apply(v1: Tree) = q"JsUnit"
    def isDefinedAt(x: Tree) = isUnit(x)
  }
  protected object jsNullLit extends PartialFunction[Tree, Tree] {
    def apply(v1: Tree) = q"JsNull"
    def isDefinedAt(x: Tree) = isNull(x)
  }

  protected val jsLit: ToExpr[JsLit] = {
    jsStringLit orElse jsNumLit orElse jsBoolLit orElse jsNullLit orElse jsUnitLit
  }

  protected lazy val jsThis: ToExpr[JsIdent] = {
    case This(name) => q"""JsIdent("this")"""
  }

  protected lazy val jsIdent: ToExpr[JsIdent] = {
    case Ident(name) =>q"JsIdent(${name.decoded})"
  }

  protected lazy val jsJStringExpr: ToExpr[JsExpr] = {
    case Apply(Select(New(Select(Select(Ident(Name("org")), Name("jscala")), Name("JString"))), _), List(Literal(Constant(str: String)))) =>
      q"JsString($str)"
  }
}
