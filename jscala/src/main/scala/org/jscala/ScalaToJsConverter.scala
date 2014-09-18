package org.jscala

import language.implicitConversions
import language.experimental.macros
import scala.reflect.macros.Context
import scala.collection.generic.{SeqFactory, MapFactory}
import scala.reflect.internal.Flags

class ScalaToJsConverter[C <: Context](val c: C, debug: Boolean) extends JsBasis[C] {
  import c.universe._

  private val traits = collection.mutable.HashMap[String, List[Tree]]()
  private val ints = Set("Byte", "Short", "Int", "Long", "Float", "Double")

  def convert(tree: Tree): Tree = {
    if (debug) println(tree)
    if (debug) println(tree.raw)

    lazy val jsSelect: ToExpr[JsExpr] = {
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

    lazy val jsUnaryOp: ToExpr[JsUnOp] = {
      case Select(q, n) if encodedUnaryOpsMap.contains(n) =>
        val op = encodedUnaryOpsMap(n)
        q"org.jscala.JsUnOp($op, ${jsExprOrDie(q)})"
    }

    def funParams(args: List[Tree]): Tree = {
      val filteredDefaults = args collect {
        case arg@Select(_, n) if n.decodedName.toString.contains("$default$") => None
        case arg@Ident(n) if n.decodedName.toString.contains("$default$") => None
        case Typed(exp, _) => Some(jsExprOrDie(exp))
        case arg => Some(jsExprOrDie(arg))
      }
      listToExpr(filteredDefaults.flatten)
    }

    lazy val jsBinOp: ToExpr[JsBinOp] = {
      case Apply(Select(q, n), List(rhs)) if encodedBinOpsMap.contains(n) =>
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

    lazy val jsTupleExpr: PFT[(Tree, Tree)] = {
      case Apply(TypeApply(Select(Apply(TypeApply(path, _), List(lhs)), arrow), _), List(rhs))
        if path.isArrow && arrow.isArrow => lhs -> rhs
      case Apply(TypeApply(path, _), List(lhs, rhs)) if path.is("scala.Tuple2.apply") => lhs -> rhs
    }

    def genMap(args: List[Tree]) = {
      val fields = for (arg <- args) yield {
        val (lhs, rhs) = jsTupleExpr(arg)
        jsString.applyOrElse(lhs, (t: Tree) => c.abort(arg.pos, "Map key type can only be String")) -> jsExprOrDie(rhs)
      }
      val params = listToExpr(fields.map { case (n, v) => q"($n, $v)" })
      q"org.jscala.JsAnonObjDecl($params)"
    }

    lazy val jsMapExpr: ToExpr[JsExpr] = {
      case Apply(TypeApply(Select(path, Name("apply")), _), args) if path.tpe.baseClasses.contains(mapFactorySym) =>
        genMap(args)
      case Apply(Select(path, Name("apply")), List(index)) if path.tpe.baseClasses.contains(mapSym) =>
        q"org.jscala.JsAccess(${jsExprOrDie(path)}, ${jsExprOrDie(index)})"
      case Apply(Select(path, Name("update")), List(key, value)) if path.tpe.baseClasses.contains(mapSym) =>
        q"""org.jscala.JsBinOp("=", org.jscala.JsAccess(${jsExprOrDie(path)}, ${jsExprOrDie(key)}), ${jsExprOrDie(value)})"""
    }

    lazy val jsForStmt: ToExpr[JsStmt] = {
      /*
        for (index <- from until untilExpr) body
        for (index <- from to untilExpr) body
      */
      case Apply(TypeApply(Select(Apply(Select(Apply(fn, List(from)), n@Name("until"|"to")), List(endExpr)), Name("foreach")), _),
      List(Function(List(ValDef(_, Name(index), _, _)), body))) if fn.is("scala.Predef.intWrapper") =>
        val forBody = jsStmtOrDie(body)
        val fromExpr = jsExprOrDie(from)
        val init = q"varDef($index, $fromExpr)"
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
        val init = q"org.jscala.JsVarDef(List($coll -> $collExpr, $idx -> org.jscala.JsNum(0, false), $ident -> org.jscala.JsAccess($seq, org.jscala.JsIdent($idx))))"
        val check = q"""org.jscala.JsBinOp("<", org.jscala.JsIdent($idx), $len)"""
        val update = q"""org.jscala.JsBinOp("=", org.jscala.JsIdent($ident), org.jscala.JsAccess(org.jscala.JsIdent($coll), org.jscala.JsUnOp("++", org.jscala.JsIdent($idx))))"""
        val forBody = jsStmtOrDie(body)
        q"org.jscala.JsFor(List($init), $check, List($update), $forBody)"
    }

    def forStmt(ident: String, coll: String, body: Tree) = {
      val idx = s"${ident}Idx"
      val seq = q"org.jscala.JsIdent($coll)"
      val len = q"""org.jscala.JsSelect($seq, "length")"""
      val init = q"org.jscala.JsVarDef(List($idx -> org.jscala.JsNum(0, false), $ident -> org.jscala.JsAccess($seq, org.jscala.JsIdent($idx))))"
      val check = q"""org.jscala.JsBinOp("<", org.jscala.JsIdent($idx), $len)"""
      val update = q"""org.jscala.JsBinOp("=", org.jscala.JsIdent($ident), org.jscala.JsAccess(org.jscala.JsIdent($coll), org.jscala.JsUnOp("++", org.jscala.JsIdent($idx))))"""
      val forBody = jsStmtOrDie(body)
      q"org.jscala.JsFor(List($init), $check, List($update), $forBody)"
    }

    lazy val jsSeqExpr: ToExpr[JsExpr] = {
      case t if t.tpe.baseClasses.contains(seqSym) => jsExpr(t)
    }

    lazy val jsIterableExpr: ToExpr[JsExpr] = jsSeqExpr orElse jsArrayIdentOrExpr

    lazy val jsArrayIdentOrExpr: ToExpr[JsExpr] = jsArrayIdent orElse jsArrayExpr

    lazy val jsArrayIdent: ToExpr[JsIdent] = {
      case i @ Ident(name) if i.tpe.baseClasses.contains(arraySym) => q"org.jscala.JsIdent(${name.decodedName.toString})"
    }

    lazy val jsArrayExpr: ToExpr[JsExpr] = {
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
      case Apply(Select(New(AppliedTypeTree(Ident(TypeName("Array")), _)), ctor), List(Literal(Constant(_)))) if ctor == nme.CONSTRUCTOR =>
        q"org.jscala.JsArray(Nil)"
      // new Array[Int](256)
      case Apply(Select(a@New(t@TypeTree()), ctor), List(Literal(Constant(_))))
        if ctor == nme.CONSTRUCTOR && t.original.isInstanceOf[AppliedTypeTree @unchecked] && t.original.asInstanceOf[AppliedTypeTree].tpt.equalsStructure(Select(Ident(TermName("scala")), TypeName("Array"))) =>
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

    def typeConverter(tpe: Tree): String = {
      val or = tpe.asInstanceOf[TypeTree].original
      val ident = or match {
        case Select(Select(This(TypeName("scala")), Name("Predef")), Name("String")) => "String"
        case Select(Ident(Name("scala")), Name(int)) if ints.contains(int)  => "Number"
        case Select(q, Name(nm)) => nm
        case Ident(Name(nm)) => nm
        case t => c.abort(c.enclosingPosition, s"Unknown type: ${show(t)}: ${showRaw(t)}")
      }
      ident
    }


    lazy val jsGlobalFuncsExpr: ToExpr[JsExpr] = {
      case Select(Apply(path, List(arg)), Name("jstr")) => jsExprOrDie(arg)
      case Apply(Select(Apply(jsAnyOps, List(expr)), Name("instanceof")), List(Literal(Constant(tpname: String)))) if jsAnyOps.is("org.jscala.package.JsAnyOps") =>
        q"""org.jscala.JsBinOp("instanceof", ${jsExprOrDie(expr)}, org.jscala.JsIdent($tpname))"""
      case TypeApply(Select(Apply(jsAnyOps, List(expr)), Name("instanceof")), List(tpe)) if jsAnyOps.is("org.jscala.package.JsAnyOps") =>
        q"""org.jscala.JsBinOp("instanceof", ${jsExprOrDie(expr)}, org.jscala.JsIdent(${typeConverter(tpe)}))"""
      case TypeApply(Select(Apply(jsAnyOps, List(expr)), Name("as")), _) if jsAnyOps.is("org.jscala.package.JsAnyOps") =>
        jsExprOrDie(expr)
      case Select(expr, Name("toDouble")) => jsExprOrDie(expr)
      case TypeApply(Select(expr, Name("asInstanceOf")), _) =>
        jsExprOrDie(expr)
      case Apply(TypeApply(Select(path, Name(n)), _), List(expr)) if path.is("org.jscala.package") && n.startsWith("implicit") =>
        jsExprOrDie(expr)
      case Apply(Select(path, Name(n)), List(expr)) if path.is("org.jscala.package") && n.startsWith("implicit") =>
        jsExprOrDie(expr)
      case Apply(path, List(Literal(Constant(js: String)))) if path.is("org.jscala.package.include") =>
        q"org.jscala.JsRaw($js)"
      case app@Apply(path, List(ident)) if path.is("org.jscala.package.include") =>
        q"org.jscala.JsLazy(() => ${jsAst(ident)})"
      case app@Apply(Apply(TypeApply(path, _), List(ident)), List(jss)) if path.is("org.jscala.package.inject") =>
        val call = c.Expr[JsExpr](Apply(jss, List(ident)))
        q"org.jscala.JsLazy(() => $call)"
      case Apply(Select(path, fn), args) if path.is("org.jscala.package") =>
        val params = funParams(args)
        q"org.jscala.JsCall(org.jscala.JsIdent(${fn.decodedName.toString}), $params)"
    }

    lazy val jsStringInterpolation: ToExpr[JsExpr] = {
      case q"scala.StringContext.apply(..$args).s(..$exprs)" =>
        val at = args.map(a => q"org.jscala.JsString($a)")
        val es = exprs.map(e => jsExprOrDie(e))
        val ls = es.zip(at.tail).flatMap { case (e, a) => List(e, a) }
        val r = ls.foldLeft(at.head){case (r, a) => q"""org.jscala.JsBinOp("+", $r, $a)"""}
        r
    }


    lazy val jsStringHelpersExpr: ToExpr[JsExpr] = {
      case q"$str.length()" if str.tpe.widen =:= typeOf[String] =>
        q"""org.jscala.JsSelect(${jsExprOrDie(str)}, "length")"""
    }


    lazy val jsNewExpr: ToExpr[JsExpr] = {
      case Apply(Select(New(Ident(ident)), _), args) =>
        val params = funParams(args)
        q"org.jscala.JsNew(org.jscala.JsCall(org.jscala.JsIdent(${ident.decodedName.toString}), $params))"
      case Apply(Select(New(path), _), args) =>
        val params = funParams(args)
        q"org.jscala.JsNew(org.jscala.JsCall(${jsExprOrDie(path)}, $params))"
    }

    lazy val jsCallExpr: ToExpr[JsExpr] = {
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
      case Apply(fun, args) =>
        val callee = jsExprOrDie apply fun
        val params = funParams(args)
        q"org.jscala.JsCall($callee, $params)"
    }

    lazy val jsIfStmt: ToExpr[JsIf] = {
      case If(cond, thenp, elsep) =>
        val condJsExpr = jsExprOrDie(cond)
        val thenJsExpr = jsStmtOrDie(thenp)
        val elseJsStmt = if (isUnit(elsep)) q"None" else q"Some(${jsStmtOrDie(elsep)})"
        q"org.jscala.JsIf($condJsExpr, $thenJsExpr, $elseJsStmt)"
    }

    lazy val jsTernaryExpr: ToExpr[JsTernary] = {
      case If(cond, thenp, elsep) if !thenp.tpe.=:=(typeOf[Unit]) && !isUnit(elsep) && jsExpr.isDefinedAt(thenp) && jsExpr.isDefinedAt(elsep) =>
        val condJsExpr = jsExprOrDie(cond)
        val thenJsExpr = jsExprOrDie(thenp)
        val elseExpr = jsExprOrDie(elsep)
        q"org.jscala.JsTernary($condJsExpr, $thenJsExpr, $elseExpr)"
    }

    lazy val jsWhileStmt: ToExpr[JsWhile] = {
      case LabelDef(termName, Nil, If(cond, Block(List(body), _), _)) if termName.encoded.startsWith("while$") =>
        val condJsExpr = jsExprOrDie(cond)
        val bodyJsStmt = jsStmtOrDie(body)
        q"org.jscala.JsWhile($condJsExpr, $bodyJsStmt)"
    }

    def addAssign(tree: Tree, name: Name) = tree match {
      case Block(stats, expr) => Block(stats :+ Assign(Ident(name), expr), q"()")
      case expr => Block(Assign(Ident(name), expr) :: Nil, q"()")
    }

    lazy val jsIfExpr: PartialFunction[(Name, Tree), Tree] = {
      case (name, If(cond, thenp, elsep)) =>
        val condJsExpr = jsExprOrDie(cond)
        val thenJsExpr = jsStmtOrDie(addAssign(thenp, name))
        val elseJsExpr = jsStmtOrDie(addAssign(elsep, name))
        q"org.jscala.JsIf($condJsExpr, $thenJsExpr, Some($elseJsExpr))"
    }

    lazy val jsMatchExpr: PartialFunction[(Name, Tree), Tree] = {
      case (name, Match(expr, cases)) => jsSwitchGen(expr, cases, body => addAssign(body, name))
    }

    lazy val jsVarDefStmt: ToExpr[JsStmt] = {
      case ValDef(_, name, _, rhs) =>
        val identifier = name.decodedName.toString
        if (jsTernaryExpr.isDefinedAt(rhs)) {
          val idents = listToExpr(List(q"$identifier -> ${jsTernaryExpr(rhs)}"))
          q"org.jscala.JsVarDef($idents)"
        } else {
          val funcs = Seq(jsIfExpr, jsMatchExpr).reduceLeft(_ orElse _) andThen { expr =>
            val ident = listToExpr(List(q"$identifier -> org.jscala.JsUnit"))
            q"org.jscala.JsStmts(List(org.jscala.JsVarDef($ident), $expr))"
          }
          val x = name -> rhs
          funcs.applyOrElse(x, (t: (TermName, Tree)) => {
            val ident = listToExpr(List(q"$identifier -> ${jsExprOrDie(rhs)}"))
            q"org.jscala.JsVarDef($ident)"
          })
        }

    }

    lazy val jsFunBody: ToExpr[JsBlock] = {
      case lit@Literal(_) =>
        val body = if (isUnit(lit)) Nil else List(jsReturnStmt(lit))
        q"org.jscala.JsBlock(${listToExpr(body)})"
      case b@Block(stmts, expr) =>
        val lastExpr = if (isUnit(expr)) Nil
        else if (expr.tpe =:= typeOf[Unit]) List(jsStmtOrDie(expr))
        else List(jsReturn(expr))
        val ss = listToExpr(stmts.map(jsStmtOrDie) ::: lastExpr)
        q"org.jscala.JsBlock($ss)"
      case rhs =>
        if (rhs.tpe =:= typeOf[Unit]) q"org.jscala.JsBlock(List(${jsStmtOrDie(rhs)}))"
        else q"org.jscala.JsBlock(List(${jsReturn(rhs)}))"
    }

    lazy val jsFunDecl: ToExpr[JsFunDecl] = {
      case DefDef(_, name, _, vparamss, _, rhs) =>
        val ident = name.decodedName.toString
        val a = vparamss.headOption.map(vp => vp.map(v => q"${v.name.decodedName.toString}")).getOrElse(Nil)
        val params = listToExpr(a)
        val body = jsFunBody(rhs)
        q"org.jscala.JsFunDecl($ident, $params, $body)"
    }

    lazy val jsAnonFunDecl: ToExpr[JsAnonFunDecl] = {
      case Block(Nil, Function(vparams, rhs)) =>
        val params = listToExpr(vparams.map(v => q"${v.name.decodedName.toString}"))
        val body = jsFunBody(rhs)
        q"org.jscala.JsAnonFunDecl($params, $body)"
      case Function(vparams, rhs) =>
        val params = listToExpr(vparams.map(v => q"${v.name.decodedName.toString}"))
        val body = jsFunBody(rhs)
        q"org.jscala.JsAnonFunDecl($params, $body)"
    }

    lazy val jsTry: ToExpr[JsTry] = {
      case Try(body, catchBlock, finBody) =>
        val ctch = catchBlock match {
          case Nil => q"None"
          case List(CaseDef(Bind(pat, _), EmptyTree, catchBody)) =>
            q"Some(org.jscala.JsCatch(org.jscala.JsIdent(${pat.decodedName.toString}), ${jsStmtOrDie(catchBody)}))"
        }
        val fin = if (finBody.equalsStructure(EmptyTree)) q"None"
        else q"Some(${jsStmtOrDie(finBody)})"
        q"org.jscala.JsTry(${jsStmtOrDie(body)}, $ctch, $fin)"
    }

    lazy val jsThrowExpr: ToExpr[JsThrow] = {
      case Throw(expr) => q"org.jscala.JsThrow(${jsExprOrDie(expr)})"
    }

    def jsSwitchGen(expr: Tree, cases: List[CaseDef], f: Tree => Tree) = {
      val cs = cases collect {
        case CaseDef(const@Literal(Constant(_)), EmptyTree, body) => q"org.jscala.JsCase(List(${jsLit(const)}), ${jsStmtOrDie(f(body))})"
        case CaseDef(sel@Select(path, n), EmptyTree, body) =>
          q"org.jscala.JsCase(List(${jsExprOrDie(sel)}), ${jsStmtOrDie(f(body))})"
        case CaseDef(Alternative(xs), EmptyTree, body) =>
          val stmt = jsStmtOrDie(f(body))
          val consts = listToExpr(xs map(c => jsLit(c)))
          q"org.jscala.JsCase($consts, $stmt)"
      }
      val df = (cases collect {
        case CaseDef(Ident(nme.WILDCARD), EmptyTree, body) => q"Some(org.jscala.JsDefault(${jsStmtOrDie(f(body))}))"
      }).headOption.getOrElse(q"None")
      val css = listToExpr(cs)
      q"org.jscala.JsSwitch(${jsExprOrDie(expr)}, $css, $df)"
    }

    lazy val jsSwitch: ToExpr[JsSwitch] = {
      case Match(expr, cases) => jsSwitchGen(expr, cases, t => t)
    }

    def eligibleDef(f: DefDef) = {
      f.name != nme.CONSTRUCTOR && f.name.decodedName.toString != "$init$" && !f.mods.hasFlag(Flags.ACCESSOR.toLong.asInstanceOf[FlagSet] | Flag.DEFERRED)
    }

    lazy val objectFields: PFT[(String, Tree)] = {
      case f@DefDef(mods, n, _, argss, _, body) if eligibleDef(f) =>
        n.decodedName.toString -> jsExprOrDie(Function(argss.flatten, body))
      case ValDef(mods, n, _, rhs) if !rhs.equalsStructure(EmptyTree)
        && !mods.hasFlag(Flags.PARAMACCESSOR.toLong.asInstanceOf[FlagSet] | Flag.ABSTRACT) => n.decodedName.toString.trim -> jsExprOrDie(rhs)
    }

    lazy val jsClassDecl: ToExpr[JsStmt] = {
      case cd@ClassDef(mods, clsName, _, Template(_, _, body)) if mods.hasFlag(Flag.TRAIT) =>
        // Remember trait AST to embed its definitions in concrete class for simplicity
        traits(cd.symbol.fullName) = body
        q"org.jscala.JsUnit"
      case cd@ClassDef(_, clsName, _, t@Template(base, _, body)) =>
        val ctor = body.collect {
          case f@DefDef(mods, n, _, argss, _, Block(stats, _)) if n == nme.CONSTRUCTOR =>
            val a = argss.flatten.map(v => v.name.decodedName.toString)
            a
        }
        if (ctor.size != 1) c.abort(c.enclosingPosition, "Only single primary constructor is currently supported. Sorry.")
        val init = ctor.head.map(f => f -> q"org.jscala.JsIdent($f)")
        val inherited = t.tpe.baseClasses.map(_.fullName).flatMap {bc => traits.get(bc).toList }
        val bigBody = inherited.foldRight(List[Tree]())(_ ::: _) ::: body
        val defs = bigBody.collect(objectFields)
        val fields = init ::: defs
        val fs = listToExpr(fields.map { case (n, v) => q"($n, $v)" })
        val args = listToExpr(ctor.head.map(arg => q"$arg"))
        val ctorBody = listToExpr(bigBody.collect {
          case _: ValDef => None
          case _: DefDef => None
          case stmt => Some(stmt)
        }.flatten.map(jsStmtOrDie))
        val ctorFuncDecl = q"org.jscala.JsFunDecl(${clsName.decodedName.toString}, $args, org.jscala.JsBlock($ctorBody))"
        q"org.jscala.JsObjDecl(${clsName.decodedName.toString}, $ctorFuncDecl, $fs)"
    }

    lazy val jsAnonObjDecl: ToExpr[JsAnonObjDecl] = {
      case Block(List(ClassDef(_, clsName, _, Template(_, _, body))), _/* Constructor call */) =>
        val defs = body.collect(objectFields)
        val params = listToExpr(defs.map { case (n, v) => q"($n, $v)" })
        q"org.jscala.JsAnonObjDecl($params)"
    }

    lazy val jsReturn1: ToExpr[JsStmt] = {
      case Return(expr) => q"org.jscala.JsReturn(${jsExprOrDie(expr)})"
    }
    lazy val jsReturn: ToExpr[JsStmt] = jsReturnStmt orElse jsStmtOrDie
    lazy val jsReturnStmt: ToExpr[JsReturn] = jsExpr andThen (jsExpr => q"org.jscala.JsReturn($jsExpr)")

    lazy val jsBlock: ToExpr[JsBlock] = {
      case Block(stmts, expr) =>
        val stmtTrees = if (expr.equalsStructure(q"()")) stmts else stmts :+ expr
        val ss = listToExpr(stmtTrees map jsStmtOrDie)
        q"org.jscala.JsBlock($ss)"
    }

    def die(msg: String) = new ToExpr[Nothing] {

      def isDefinedAt(x: Tree) = true

      def apply(v1: Tree) = {
        val stack = if (debug) Seq("Raw tree:", v1.raw) ++ Thread.currentThread.getStackTrace mkString "\n" else ""
        c.abort(tree.pos, s"$msg: $v1. $stack")
      }
    }

    lazy val jsExpr: ToExpr[JsExpr] = Seq(
      jsLit,
      jsUnaryOp,
      jsBinOp,
      jsGlobalFuncsExpr,
      jsStringHelpersExpr,
      jsStringInterpolation,
      jsJStringExpr,
      jsArrayExpr,
      jsNewExpr,
      jsMapExpr,
      jsCallExpr,
      jsAnonFunDecl,
      jsSelect,
      jsIdent,
      jsThis,
      jsAnonObjDecl,
      jsThrowExpr,
      jsTernaryExpr
    ) reduceLeft( _ orElse _)

    lazy val jsExprOrDie: ToExpr[JsExpr] = jsExpr orElse die("Unsupported syntax")

    lazy val jsStmt: ToExpr[JsStmt] = Seq(
      jsBlock,
      jsVarDefStmt,
      jsIfStmt,
      jsSwitch,
      jsWhileStmt,
      jsTry,
      jsForStmt,
      jsFunDecl,
      jsClassDecl,
      jsReturn1,
      jsExpr
    ) reduceLeft (_ orElse _)

    lazy val jsStmtOrDie: ToExpr[JsStmt] = jsStmt orElse die("Unsupported syntax")

    lazy val jsAst: ToExpr[JsAst] = jsStmtOrDie

    val expr = jsAst apply tree

    val resultTree = if (!tree.tpe.=:=(typeOf[Nothing]) && functionTypes.exists(tree.tpe.<:<)) {
      q"${expr}.asInstanceOf[org.jscala.JsAst with ${tree.tpe}]"
    } else expr

    resultTree
  }
}
