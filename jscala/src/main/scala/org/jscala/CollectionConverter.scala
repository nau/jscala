package org.jscala

import scala.reflect.macros.blackbox

/**
 * Author: Alexander Nemish
 */
trait CollectionConverter[C <: blackbox.Context] extends BasisConverter[C] {
  import c.universe._

  private lazy val jsSeqExpr: ToExpr[JsExpr] = {
    case t if t.tpe.baseClasses.contains(seqSym) => jsExpr(t)
  }

  protected lazy val jsIterableExpr: ToExpr[JsExpr] = jsSeqExpr orElse jsArrayIdentOrExpr

  private lazy val jsArrayIdentOrExpr: ToExpr[JsExpr] = jsArrayIdent orElse jsArrayExpr

  private lazy val jsArrayIdent: ToExpr[JsIdent] = {
    case i @ Ident(name) if i.tpe.baseClasses.contains(arraySym) => q"org.jscala.JsIdent(${name.decodedName.toString})"
  }

  protected lazy val jsArrayExpr: ToExpr[JsExpr] = {
    // Array creation
    case Apply(TypeApply(path, _), args) if path.is("org.jscala.JArray.apply") =>
      val params = listToExpr(args map jsExprOrDie)
      q"org.jscala.JsArray($params)"
    case TypeApply(path, args) if path.is("scala.Array.apply") =>
      val params = listToExpr(args map jsExprOrDie)
      q"org.jscala.JsArray($params)"
    case Apply(Apply(TypeApply(path, _), args), _) if path.is("scala.Array.apply") =>
      val params = listToExpr(args map jsExprOrDie)
      q"org.jscala.JsArray($params)"
    case TypeApply(path, args) if path.is("scala.Array.apply") =>
      val params = listToExpr(args map jsExprOrDie)
      q"org.jscala.JsArray($params)"
    case Apply(path, args) if path.is("scala.Array.apply") =>
      val params = listToExpr(args map jsExprOrDie)
      q"org.jscala.JsArray($params)"
    case Apply(Ident(Name("Array")), args) =>
      val params = listToExpr(args map jsExprOrDie)
      q"org.jscala.JsArray($params)"
    case Apply(Select(New(AppliedTypeTree(Ident(TypeName("Array")), _)), ctor), List(Literal(Constant(_)))) if ctor == termNames.CONSTRUCTOR =>
      q"org.jscala.JsArray(Nil)"
    // new Array[Int](256)
    case Apply(Select(a@New(t@TypeTree()), ctor), List(Literal(Constant(_))))
      if ctor == termNames.CONSTRUCTOR && t.original.isInstanceOf[AppliedTypeTree @unchecked] && t.original.asInstanceOf[AppliedTypeTree].tpt.equalsStructure(Select(Ident(TermName("scala")), TypeName("Array"))) =>
      q"org.jscala.JsArray(Nil)"
    case Apply(Select(a@New(t@TypeTree()), ctor), List(Literal(Constant(_)))) =>
      q"org.jscala.JsArray(Nil)"
    case Apply(TypeApply(Select(path, Name("apply")), _), args) if path.tpe.baseClasses.contains(seqFactorySym) =>
      val params = listToExpr(args map jsExprOrDie)
      q"org.jscala.JsArray($params)"
    // Array access
    case Apply(Select(path, Name("apply")), List(idx)) if isArray(path) =>
      q"org.jscala.JsAccess(${jsExprOrDie(path)}, ${jsExprOrDie(idx)})"
    // Array update
    case Apply(Select(path, Name("update")), List(key, value)) if isArray(path) =>
      q"""org.jscala.JsBinOp("=", org.jscala.JsAccess(${jsExprOrDie(path)}, ${jsExprOrDie(key)}), ${jsExprOrDie(value)})"""
    // arrayOps
    case Apply(TypeApply(path, _), List(body)) if path.is("scala.Predef.refArrayOps") => jsArrayIdentOrExpr(body)
    case Apply(Select(Select(This(TypeName("scala")), Name("Predef")), Name(ops)), List(body)) if ops.endsWith("ArrayOps") => jsArrayIdentOrExpr(body)
    case Apply(Select(Select(Ident("scala"), Name("Predef")), Name(ops)), List(body)) if ops.endsWith("ArrayOps") => jsArrayIdentOrExpr(body)
    // Tuples
    case Apply(TypeApply(Select(Select(Ident(Name("scala")), Name(tuple)), Name("apply")), _), args) if tuple.contains("Tuple") =>
      val params = listToExpr(args map jsExprOrDie)
      q"org.jscala.JsArray($params)"
  }

  private def genMap(args: List[Tree]) = {
    val fields = for (arg <- args) yield {
      val (lhs, rhs) = jsTupleExpr(arg)
      jsString.applyOrElse(lhs, (t: Tree) => c.abort(arg.pos, "Map key type can only be String")) -> jsExprOrDie(rhs)
    }
    val params = listToExpr(fields.map { case (n, v) => q"($n, $v)" })
    q"org.jscala.JsAnonObjDecl($params)"
  }


  protected lazy val jsMapExpr: ToExpr[JsExpr] = {
    case Apply(TypeApply(Select(path, Name("apply")), _), args) if path.tpe.baseClasses.contains(mapFactorySym) =>
      genMap(args)
    case Apply(Select(path, Name("apply")), List(index)) if path.tpe.baseClasses.contains(mapSym) =>
      q"org.jscala.JsAccess(${jsExprOrDie(path)}, ${jsExprOrDie(index)})"
    case Apply(Select(path, Name("update")), List(key, value)) if path.tpe.baseClasses.contains(mapSym) =>
      q"""org.jscala.JsBinOp("=", org.jscala.JsAccess(${jsExprOrDie(path)}, ${jsExprOrDie(key)}), ${jsExprOrDie(value)})"""
  }

  protected lazy val jsForStmt: ToExpr[JsStmt] = {
    /*
      for (index <- from until untilExpr) body
      for (index <- from to untilExpr) body
    */
    case Apply(TypeApply(Select(Apply(Select(Apply(fn, List(from)), n@Name("until"|"to")), List(endExpr)), Name("foreach")), _),
    List(Function(List(ValDef(_, Name(index), _, _)), body))) if fn.is("scala.Predef.intWrapper") =>
      val forBody = jsStmtOrDie(body)
      val fromExpr = jsExprOrDie(from)
      val init = q"org.jscala.JsVarDef($index, $fromExpr)"
      val check = if (n.decodedName.toString == "until")
        q"""org.jscala.JsBinOp("<", org.jscala.JsIdent($index), ${jsExprOrDie(endExpr)})"""
      else q"""org.jscala.JsBinOp("<=", org.jscala.JsIdent($index), ${jsExprOrDie(endExpr)})"""
      val update = q"""org.jscala.JsUnOp("++", org.jscala.JsIdent($index))"""
      q"org.jscala.JsFor(List($init), $check, List($update), $forBody)"
    /*
      val coll = Seq(1, 2)
      for (ident <- coll) body
    */
    case Apply(TypeApply(Select(Apply(TypeApply(Select(path, Name(n)), _), List(Ident(Name(coll)))), Name("foreach")), _), List(Function(List(ValDef(_, Name(ident), _, _)), body)))
      if path.is("org.jscala.package") && n.startsWith("implicit") => forStmt(ident, coll, body)
    /*
      forIn(ident, coll) body
     */
    case app@Apply(Apply(TypeApply(path, _), List(coll)), List(Function(List(ValDef(_, Name(ident), _, _)), body))) if path.is("org.jscala.package.forIn") =>
      q"org.jscala.JsForIn(org.jscala.JsIdent($ident), ${jsExprOrDie(coll)}, ${jsStmtOrDie(body)})"

    /*
      coll.foreach(item)
     */
    case Apply(TypeApply(Select(collTree, Name("foreach")), _), List(Function(List(ValDef(_, Name(ident), _, _)), body))) if jsIterableExpr.isDefinedAt(collTree) =>
      val collExpr = jsIterableExpr(collTree)
      val coll = s"${ident}Coll"
      val idx = s"${ident}Idx"
      val seq = q"org.jscala.JsIdent($coll)"
      val len = q"""org.jscala.JsSelect($seq, "length")"""
      val init = q"List(org.jscala.JsVarDef($coll, $collExpr), org.jscala.JsVarDef($idx, org.jscala.JsNum(0, false)), org.jscala.JsVarDef($ident, org.jscala.JsAccess($seq, org.jscala.JsIdent($idx))))"
      val check = q"""org.jscala.JsBinOp("<", org.jscala.JsIdent($idx), $len)"""
      val update = q"""org.jscala.JsBinOp("=", org.jscala.JsIdent($ident), org.jscala.JsAccess(org.jscala.JsIdent($coll), org.jscala.JsUnOp("++", org.jscala.JsIdent($idx))))"""
      val forBody = jsStmtOrDie(body)
      q"org.jscala.JsFor($init, $check, List($update), $forBody)"
  }

  private def forStmt(ident: String, coll: String, body: Tree) = {
    val idx = s"${ident}Idx"
    val seq = q"org.jscala.JsIdent($coll)"
    val len = q"""org.jscala.JsSelect($seq, "length")"""
    val init = q"List(org.jscala.JsVarDef($idx, org.jscala.JsNum(0, false)), org.jscala.JsVarDef($ident, org.jscala.JsAccess($seq, org.jscala.JsIdent($idx))))"
    val check = q"""org.jscala.JsBinOp("<", org.jscala.JsIdent($idx), $len)"""
    val update = q"""org.jscala.JsBinOp("=", org.jscala.JsIdent($ident), org.jscala.JsAccess(org.jscala.JsIdent($coll), org.jscala.JsUnOp("++", org.jscala.JsIdent($idx))))"""
    val forBody = jsStmtOrDie(body)
    q"org.jscala.JsFor($init, $check, List($update), $forBody)"
  }
}
