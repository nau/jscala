package org.jscala

import scala.reflect.macros.blackbox

/**
 * Author: Alexander Nemish
 * Date: 10/25/13
 * Time: 10:50 PM
 */
trait BasisConverter[C <: blackbox.Context] extends MacroHelpers[C] {
  import c.universe._

  class DieException(val msg: String, val t: Tree) extends RuntimeException

  protected def die(msg: String): ToExpr[Nothing] = new ToExpr[Nothing] {

    def isDefinedAt(x: Tree): Boolean = true

    def apply(v1: Tree): Nothing = throw new DieException(msg, v1)
  }

  protected def funParams(args: List[Tree]): Tree = {
    val filteredDefaults = args collect {
      case arg@Select(_, n) if n.decodedName.toString.contains("$default$") => None
      case arg@Ident(n) if n.decodedName.toString.contains("$default$") => None
      case Typed(exp, _) => Some(jsExprOrDie(exp))
      case arg => Some(jsExprOrDie(arg))
    }
    listToExpr(filteredDefaults.flatten)
  }

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

  protected lazy val jsSelect: ToExpr[JsExpr] = {
    case Select(Select(Select(Ident(Name("scala")), Name("scalajs")), Name("js")), Name(name)) => q"""org.jscala.JsIdent($name)"""
    //      case q"scala.scalajs.js.$name" => q"""org.jscala.JsIdent($name)"""
    case q"org.scalajs.dom.`package`.${TermName(name)}" => q"""org.jscala.JsIdent($name)"""
    // org.jscala.package.$ident => $ident
    case Select(Select(Select(Ident(Name("org")), Name("jscala")), Name("package")), Name(name)) =>
      q"org.jscala.JsIdent($name)"
    // org.jscala.$ident => $ident
    case Select(Select(Ident(Name("org")), Name("jscala")), Name(name)) =>
      q"org.jscala.JsIdent($name)"
    // objectname.$ident => $ident
    case s@Select(q@Ident(_), name) if q.symbol.isModule => jsExprOrDie(Ident(name))
    case Select(q, name) =>
      q"org.jscala.JsSelect(${jsExprOrDie(q)}, ${name.decodedName.toString})"
  }

  protected lazy val jsBlock: ToExpr[JsBlock] = {
    case Block(stmts, expr) =>
      val stmtTrees = if (expr.equalsStructure(q"()")) stmts else stmts :+ expr
      val ss = listToExpr(stmtTrees map jsStmtOrDie)
      q"org.jscala.JsBlock($ss)"
  }

  protected lazy val jsUnaryOp: ToExpr[JsUnOp] = {
    case q"$q.$n" if encodedUnaryOpsMap.contains(n) =>
      val op = encodedUnaryOpsMap(n)
      q"org.jscala.JsUnOp($op, ${jsExprOrDie(q)})"
  }

  protected lazy val jsBinOp: ToExpr[JsBinOp] = {
    case q"$q.$n($rhs)" if encodedBinOpsMap.contains(n) =>
      val op = encodedBinOpsMap(n)
      val opExpr = q"$op"
      val qExpr = jsExprOrDie(q)
      val rhsExpr = jsExprOrDie(rhs)
      // generate correct whole number devision JavaScript if a and b are [Byte,Short,Int,Long]: a/b|0
      if (op == "/" && q.isNum && rhs.isNum)
        q"""org.jscala.JsBinOp("|", org.jscala.JsBinOp($opExpr, $qExpr, $rhsExpr), org.jscala.JsNum(0, false))"""
      else q"org.jscala.JsBinOp($opExpr, $qExpr, $rhsExpr)"
    case Assign(lhs, rhs) =>q"""org.jscala.JsBinOp("=", ${jsExprOrDie(lhs)}, ${jsExprOrDie(rhs)})"""
  }

  protected lazy val jsTupleExpr: PFT[(Tree, Tree)] = {
    case Apply(TypeApply(Select(Apply(TypeApply(path, _), List(lhs)), arrow), _), List(rhs))
      if path.isArrow && arrow.isArrow => lhs -> rhs
    case Apply(TypeApply(path, _), List(lhs, rhs)) if path.is("scala.Tuple2.apply") => lhs -> rhs
  }

  protected lazy val jsStringInterpolation: ToExpr[JsExpr] = {
    case q"scala.StringContext.apply(..$args).s(..$exprs)" =>
      val at = args.map(a => q"org.jscala.JsString($a)")
      val es = exprs.map(e => jsExprOrDie(e))
      val ls = es.zip(at.tail).flatMap { case (e, a) => List(e, a) }
      val r = ls.foldLeft(at.head){case (r, a) => q"""org.jscala.JsBinOp("+", $r, $a)"""}
      r
  }

  protected lazy val jsStringHelpersExpr: ToExpr[JsExpr] = {
    case q"$str.length()" if str.tpe.widen =:= typeOf[String] =>
      q"""org.jscala.JsSelect(${jsExprOrDie(str)}, "length")"""
  }


  protected lazy val jsNewExpr: ToExpr[JsExpr] = {
    case Apply(Select(New(Ident(ident)), _), args) =>
      val params = funParams(args)
      q"org.jscala.JsNew(org.jscala.JsCall(org.jscala.JsIdent(${ident.decodedName.toString}), $params))"
    case Apply(Select(New(path), _), args) =>
      val params = funParams(args)
      q"org.jscala.JsNew(org.jscala.JsCall(${jsExprOrDie(path)}, $params))"
  }

  protected lazy val jsCallExpr: ToExpr[JsExpr] = {
    case Apply(Select(lhs, name), List(rhs)) if name.decodedName.toString.endsWith("_=") =>
      q"""org.jscala.JsBinOp("=", org.jscala.JsSelect(${jsExprOrDie(lhs)}, ${name.decodedName.toString.dropRight(2)}), ${jsExprOrDie(rhs)})"""
    case Apply(Apply(Select(sel, Name("applyDynamic")), List(Literal(Constant(name: String)))), args) =>
      val callee = q"org.jscala.JsSelect(${jsExprOrDie(sel)}, $name)"
      val params = listToExpr(args.map(jsExprOrDie))
      q"org.jscala.JsCall($callee, $params)"
    case Apply(Apply(Select(sel, Name("updateDynamic")), List(Literal(Constant(name: String)))), List(arg)) =>
      val callee = q"org.jscala.JsSelect(${jsExprOrDie(sel)}, $name)"
      q"""org.jscala.JsBinOp("=", $callee, ${jsExprOrDie(arg)})"""
    case Apply(Select(sel, Name("selectDynamic")), List(Literal(Constant(name: String)))) =>
      q"org.jscala.JsSelect(${jsExprOrDie(sel)}, $name)"
    case app@Apply(fun, args) if app.tpe <:< typeOf[JsAst] =>
      val expr = c.Expr[JsAst](app)
      q"org.jscala.JsLazy(() => $expr)"
    case Apply(TypeApply(fun, _), args) =>
      val callee = jsExprOrDie apply fun
      val params = funParams(args)
      q"org.jscala.JsCall($callee, $params)"
    case Apply(fun, args) =>
      val callee = jsExprOrDie apply fun
      val params = funParams(args)
      q"org.jscala.JsCall($callee, $params)"
  }

  protected def jsExpr: ToExpr[JsExpr]

  protected val jsExprOrDie: ToExpr[JsExpr] = jsExpr orElse die("Unsupported syntax")

  protected def jsStmt: ToExpr[JsStmt]

  protected val jsStmtOrDie: ToExpr[JsStmt] = jsStmt orElse die("Unsupported syntax")
}
