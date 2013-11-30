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
  protected lazy val jsStringLit: ToExpr[JsString] = jsString.andThen(s => reify(JsString(c.literal(s).splice)))

  protected lazy val jsNumLit: ToExpr[JsNum] = {
    case Literal(Constant(value: Byte))  => reify(JsNum(c.literal(value).splice, isFloat = false))
    case Literal(Constant(value: Short))  => reify(JsNum(c.literal(value).splice, isFloat = false))
    case Literal(Constant(value: Int))  => reify(JsNum(c.literal(value).splice, isFloat = false))
    case Literal(Constant(value: Long))  => reify(JsNum(c.literal(value).splice, isFloat = false))
    case Literal(Constant(value: Double))  => reify(JsNum(c.literal(value).splice, isFloat = true))
  }
  protected lazy val jsBoolLit: ToExpr[JsBool] = {
    case Literal(Constant(value: Boolean))  => reify(JsBool(c.literal(value).splice))
  }
  protected object jsUnitLit extends PartialFunction[Tree, Expr[JsUnit.type]] {
    def apply(v1: Tree) = reify(JsUnit)
    def isDefinedAt(x: Tree) = isUnit(x)
  }
  protected object jsNullLit extends PartialFunction[Tree, Expr[JsNull.type]] {
    def apply(v1: Tree) = reify(JsNull)
    def isDefinedAt(x: Tree) = isNull(x)
  }

  protected val jsLit: ToExpr[JsLit] = {
    jsStringLit orElse jsNumLit orElse jsBoolLit orElse jsNullLit orElse jsUnitLit
  }

  protected lazy val jsThis: ToExpr[JsIdent] = {
    case This(name) => reify(JsIdent("this"))
  }

  protected lazy val jsIdent: ToExpr[JsIdent] = {
    case Ident(name) => reify(JsIdent(c.literal(name.decoded).splice))
  }

  protected lazy val jsJStringExpr: ToExpr[JsExpr] = {
    case Apply(Select(New(Select(Select(Ident(Name("org")), Name("jscala")), Name("JString"))), _), List(Literal(Constant(str: String)))) =>
      reify(JsString(c.literal(str).splice))
  }
}
