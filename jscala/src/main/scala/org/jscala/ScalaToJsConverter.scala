package org.jscala

import language.implicitConversions
import language.experimental.macros
import scala.reflect.macros.Context
import scala.collection.generic.{SeqFactory, MapFactory}
import scala.reflect.internal.Flags

class ScalaToJsConverter[C <: Context](val c: C, debug: Boolean) extends JsBasis[C] {
  import c.universe._

  private val traits = collection.mutable.HashMap[String, List[Tree]]()

  def convert(tree: Tree): Tree = {
    if (debug) println(tree)
    if (debug) println(tree.raw)

    lazy val jsSelect: ToExpr[JsExpr] = {
      // org.scala.package.$ident => $ident
      case Select(Select(Select(Ident(Name("org")), Name("jscala")), Name("package")), Name(name)) =>
        q"JsIdent($name)"
      // org.scala.$ident => $ident
      case Select(Select(Ident(Name("org")), Name("jscala")), Name(name)) =>
        q"JsIdent($name)"
      // objectname.$ident => $ident
      case s@Select(q@Ident(_), name) if q.symbol.isModule => jsExprOrDie(Ident(name))
      case Select(q, name) =>
        q"JsSelect(${jsExprOrDie(q)}, ${name.decodedName.toString})"
    }

    lazy val jsUnaryOp: ToExpr[JsUnOp] = {
      case Select(q, n) if encodedUnaryOpsMap.contains(n) =>
        val op = encodedUnaryOpsMap(n)
        q"JsUnOp($op, ${jsExprOrDie(q)})"
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
          q"""JsBinOp("|", JsBinOp($opExpr, $qExpr, $rhsExpr), JsNum(0, false))"""
        else q"JsBinOp($opExpr, $qExpr, $rhsExpr)"
      case Assign(lhs, rhs) =>q"""JsBinOp("=", ${jsExprOrDie(lhs)}, ${jsExprOrDie(rhs)})"""
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
      q"JsAnonObjDecl($params)"
    }

    lazy val jsMapExpr: ToExpr[JsExpr] = {
      case Apply(TypeApply(Select(path, Name("apply")), _), args) if path.tpe.baseClasses.contains(mapFactorySym) =>
        genMap(args)
      case Apply(Select(path, Name("apply")), List(index)) if path.tpe.baseClasses.contains(mapSym) =>
        q"JsAccess(${jsExprOrDie(path)}, ${jsExprOrDie(index)})"
      case Apply(Select(path, Name("update")), List(key, value)) if path.tpe.baseClasses.contains(mapSym) =>
        q"""JsBinOp("=", JsAccess(${jsExprOrDie(path)}, ${jsExprOrDie(key)}), ${jsExprOrDie(value)})"""
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
          q"""JsBinOp("<", JsIdent($index), ${jsExprOrDie(endExpr)})"""
          else q"""JsBinOp("<=", JsIdent($index), ${jsExprOrDie(endExpr)})"""
        val update = q"""JsUnOp("++", JsIdent($index))"""
        q"JsFor(List($init), $check, List($update), $forBody)"
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
        q"JsForIn(JsIdent($ident), ${jsExprOrDie(coll)}, ${jsStmtOrDie(body)})"

      /*
        coll.foreach(item)
       */
      case Apply(TypeApply(Select(collTree, Name("foreach")), _), List(Function(List(ValDef(_, Name(ident), _, _)), body))) if jsIterableExpr.isDefinedAt(collTree) =>
        val collExpr = jsIterableExpr(collTree)
        val coll = s"${ident}Coll"
        val idx = s"${ident}Idx"
        val seq = q"JsIdent($coll)"
        val len = q"""JsSelect($seq, "length")"""
        val init = q"JsVarDef(List($coll -> $collExpr, $idx -> JsNum(0, false), $ident -> JsAccess($seq, JsIdent($idx))))"
        val check = q"""JsBinOp("<", JsIdent($idx), $len)"""
        val update = q"""JsBinOp("=", JsIdent($ident), JsAccess(JsIdent($coll), JsUnOp("++", JsIdent($idx))))"""
        val forBody = jsStmtOrDie(body)
        q"JsFor(List($init), $check, List($update), $forBody)"
    }

    def forStmt(ident: String, coll: String, body: Tree) = {
      val idx = s"${ident}Idx"
      val seq = q"JsIdent($coll)"
      val len = q"""JsSelect($seq, "length")"""
      val init = q"JsVarDef(List($idx -> JsNum(0, false), $ident -> JsAccess($seq, JsIdent($idx))))"
      val check = q"""JsBinOp("<", JsIdent($idx), $len)"""
      val update = q"""JsBinOp("=", JsIdent($ident), JsAccess(JsIdent($coll), JsUnOp("++", JsIdent($idx))))"""
      val forBody = jsStmtOrDie(body)
      q"JsFor(List($init), $check, List($update), $forBody)"
    }

    lazy val jsSeqExpr: ToExpr[JsExpr] = {
      case t if t.tpe.baseClasses.contains(seqSym) => jsExpr(t)
    }

    lazy val jsIterableExpr: ToExpr[JsExpr] = jsSeqExpr orElse jsArrayIdentOrExpr

    lazy val jsArrayIdentOrExpr: ToExpr[JsExpr] = jsArrayIdent orElse jsArrayExpr

    lazy val jsArrayIdent: ToExpr[JsIdent] = {
      case i @ Ident(name) if i.tpe.baseClasses.contains(arraySym) => q"JsIdent(${name.decodedName.toString})"
    }

    lazy val jsArrayExpr: ToExpr[JsExpr] = {
      // Array creation
      case Apply(TypeApply(path, _), args) if path.is("org.jscala.JArray.apply") =>
        val params = listToExpr(args map jsExprOrDie)
        q"JsArray($params)"
      case TypeApply(path, args) if path.is("scala.Array.apply") =>
        val params = listToExpr(args map jsExprOrDie)
        q"JsArray($params)"
      case Apply(Apply(TypeApply(path, _), args), _) if path.is("scala.Array.apply") =>
        val params = listToExpr(args map jsExprOrDie)
        q"JsArray($params)"
      case TypeApply(path, args) if path.is("scala.Array.apply") =>
        val params = listToExpr(args map jsExprOrDie)
        q"JsArray($params)"
      case Apply(path, args) if path.is("scala.Array.apply") =>
        val params = listToExpr(args map jsExprOrDie)
        q"JsArray($params)"
      case Apply(Ident(Name("Array")), args) =>
        val params = listToExpr(args map jsExprOrDie)
        q"JsArray($params)"
      case Apply(Select(New(AppliedTypeTree(Ident(TypeName("Array")), _)), ctor), List(Literal(Constant(_)))) if ctor == nme.CONSTRUCTOR =>
        q"JsArray(Nil)"
      // new Array[Int](256)
      case Apply(Select(a@New(t@TypeTree()), ctor), List(Literal(Constant(_))))
        if ctor == nme.CONSTRUCTOR && t.original.isInstanceOf[AppliedTypeTree @unchecked] && t.original.asInstanceOf[AppliedTypeTree].tpt.equalsStructure(Select(Ident(TermName("scala")), TypeName("Array"))) =>
        q"JsArray(Nil)"
      case Apply(Select(a@New(t@TypeTree()), ctor), List(Literal(Constant(_)))) =>
        q"JsArray(Nil)"
      case Apply(TypeApply(Select(path, Name("apply")), _), args) if path.tpe.baseClasses.contains(seqFactorySym) =>
        val params = listToExpr(args map jsExprOrDie)
        q"JsArray($params)"
      // Array access
      case Apply(Select(path, Name("apply")), List(idx)) if isArray(path) =>
        q"JsAccess(${jsExprOrDie(path)}, ${jsExprOrDie(idx)})"
      // Array update
      case Apply(Select(path, Name("update")), List(key, value)) if isArray(path) =>
        q"""JsBinOp("=", JsAccess(${jsExprOrDie(path)}, ${jsExprOrDie(key)}), ${jsExprOrDie(value)})"""
      // arrayOps
      case Apply(TypeApply(path, _), List(body)) if path.is("scala.Predef.refArrayOps") => jsArrayIdentOrExpr(body)
      case Apply(Select(Select(This(TypeName("scala")), Name("Predef")), Name(ops)), List(body)) if ops.endsWith("ArrayOps") => jsArrayIdentOrExpr(body)
      case Apply(Select(Select(Ident("scala"), Name("Predef")), Name(ops)), List(body)) if ops.endsWith("ArrayOps") => jsArrayIdentOrExpr(body)
      // Tuples
      case Apply(TypeApply(Select(Select(Ident(Name("scala")), Name(tuple)), Name("apply")), _), args) if tuple.contains("Tuple") =>
        val params = listToExpr(args map jsExprOrDie)
        q"JsArray($params)"
    }

    lazy val jsGlobalFuncsExpr: ToExpr[JsExpr] = {
      case Select(Apply(path, List(arg)), Name("jstr")) => jsExprOrDie(arg)
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
        q"JsRaw($js)"
      case app@Apply(Select(path, _), List(ident)) if path.is("org.jscala.package.inject") =>
        q"JsLazy(() => ${jsAst(ident)})"
      case app@Apply(Apply(TypeApply(path, _), List(ident)), List(jss)) if path.is("org.jscala.package.inject") =>
        val call = c.Expr[JsExpr](Apply(jss, List(ident)))
        q"JsLazy(() => $call)"
      case Apply(Select(path, fn), args) if path.is("org.jscala.package") =>
        val params = funParams(args)
        q"JsCall(JsIdent(${fn.decodedName.toString}), $params)"
    }

    lazy val jsStringInterpolation: ToExpr[JsExpr] = {
      case q"scala.StringContext.apply(..$args).s(..$exprs)" =>
        val at = args.map(a => q"JsString($a)")
        val es = exprs.map(e => jsExprOrDie(e))
        val ls = es.zip(at.tail).flatMap { case (e, a) => List(e, a) }
        val r = ls.foldLeft(at.head){case (r, a) => q"""JsBinOp("+", $r, $a)"""}
        r
    }


    lazy val jsStringHelpersExpr: ToExpr[JsExpr] = {
      case q"$str.length()" if str.tpe.widen =:= typeOf[String] =>
        q"""JsSelect(${jsExprOrDie(str)}, "length")"""
    }


    lazy val jsNewExpr: ToExpr[JsExpr] = {
      case Apply(Select(New(Ident(ident)), _), args) =>
        val params = funParams(args)
        q"JsNew(JsCall(JsIdent(${ident.decodedName.toString}), $params))"
      case Apply(Select(New(path), _), args) =>
        val params = funParams(args)
        q"JsNew(JsCall(${jsExprOrDie(path)}, $params))"
    }

    lazy val jsCallExpr: ToExpr[JsExpr] = {
      case Apply(Select(lhs, name), List(rhs)) if name.decodedName.toString.endsWith("_=") =>
        q"""JsBinOp("=", JsSelect(${jsExprOrDie(lhs)}, ${name.decodedName.toString.dropRight(2)}), ${jsExprOrDie(rhs)})"""
      case Apply(Apply(Select(sel, Name("applyDynamic")), List(Literal(Constant(name: String)))), args) =>
        val callee = q"JsSelect(${jsExprOrDie(sel)}, $name)"
        val params = listToExpr(args.map(jsExprOrDie))
        q"JsCall($callee, $params)"
      case Apply(Apply(Select(sel, Name("updateDynamic")), List(Literal(Constant(name: String)))), List(arg)) =>
        val callee = q"JsSelect(${jsExprOrDie(sel)}, $name)"
        q"""JsBinOp("=", $callee, ${jsExprOrDie(arg)})"""
      case Apply(Select(sel, Name("selectDynamic")), List(Literal(Constant(name: String)))) =>
        q"JsSelect(${jsExprOrDie(sel)}, $name)"
      case app@Apply(fun, args) if app.tpe <:< typeOf[JsAst] =>
        val expr = c.Expr[JsAst](app)
        q"JsLazy(() => $expr)"
      case Apply(fun, args) =>
        val callee = jsExprOrDie apply fun
        val params = funParams(args)
        q"JsCall($callee, $params)"
    }

    lazy val jsIfStmt: ToExpr[JsIf] = {
      case If(cond, thenp, elsep) =>
        val condJsExpr = jsExprOrDie(cond)
        val thenJsExpr = jsStmtOrDie(thenp)
        val elseJsStmt = if (isUnit(elsep)) q"None" else q"Some(${jsStmtOrDie(elsep)})"
        q"JsIf($condJsExpr, $thenJsExpr, $elseJsStmt)"
    }

    lazy val jsTernaryExpr: ToExpr[JsTernary] = {
      case If(cond, thenp, elsep) if !thenp.tpe.=:=(typeOf[Unit]) && !isUnit(elsep) && jsExpr.isDefinedAt(thenp) && jsExpr.isDefinedAt(elsep) =>
        val condJsExpr = jsExprOrDie(cond)
        val thenJsExpr = jsExprOrDie(thenp)
        val elseExpr = jsExprOrDie(elsep)
        q"JsTernary($condJsExpr, $thenJsExpr, $elseExpr)"
    }

    lazy val jsWhileStmt: ToExpr[JsWhile] = {
      case LabelDef(termName, Nil, If(cond, Block(List(body), _), _)) if termName.encoded.startsWith("while$") =>
        val condJsExpr = jsExprOrDie(cond)
        val bodyJsStmt = jsStmtOrDie(body)
        q"JsWhile($condJsExpr, $bodyJsStmt)"
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
        q"JsIf($condJsExpr, $thenJsExpr, Some($elseJsExpr))"
    }

    lazy val jsMatchExpr: PartialFunction[(Name, Tree), Tree] = {
      case (name, Match(expr, cases)) => jsSwitchGen(expr, cases, body => addAssign(body, name))
    }

    lazy val jsVarDefStmt: ToExpr[JsStmt] = {
      case ValDef(_, name, _, rhs) =>
        val identifier = name.decodedName.toString
        if (jsTernaryExpr.isDefinedAt(rhs)) {
          val idents = listToExpr(List(q"$identifier -> ${jsTernaryExpr(rhs)}"))
          q"JsVarDef($idents)"
        } else {
          val funcs = Seq(jsIfExpr, jsMatchExpr).reduceLeft(_ orElse _) andThen { expr =>
            val ident = listToExpr(List(q"$identifier -> JsUnit"))
            q"JsStmts(List(JsVarDef($ident), $expr))"
          }
          val x = name -> rhs
          funcs.applyOrElse(x, (t: (TermName, Tree)) => {
            val ident = listToExpr(List(q"$identifier -> ${jsExprOrDie(rhs)}"))
            q"JsVarDef($ident)"
          })
        }

    }

    lazy val jsFunBody: ToExpr[JsBlock] = {
      case lit@Literal(_) =>
        val body = if (isUnit(lit)) Nil else List(jsReturnStmt(lit))
        q"JsBlock(${listToExpr(body)})"
      case b@Block(stmts, expr) =>
        val lastExpr = if (isUnit(expr)) Nil
        else if (expr.tpe =:= typeOf[Unit]) List(jsStmtOrDie(expr))
        else List(jsReturn(expr))
        val ss = listToExpr(stmts.map(jsStmtOrDie) ::: lastExpr)
        q"JsBlock($ss)"
      case rhs =>
        if (rhs.tpe =:= typeOf[Unit]) q"JsBlock(List(${jsStmtOrDie(rhs)}))"
        else q"JsBlock(List(${jsReturn(rhs)}))"
    }

    lazy val jsFunDecl: ToExpr[JsFunDecl] = {
      case DefDef(_, name, _, vparamss, _, rhs) =>
        val ident = name.decodedName.toString
        val a = vparamss.headOption.map(vp => vp.map(v => q"${v.name.decodedName.toString}")).getOrElse(Nil)
        val params = listToExpr(a)
        val body = jsFunBody(rhs)
        q"JsFunDecl($ident, $params, $body)"
    }

    lazy val jsAnonFunDecl: ToExpr[JsAnonFunDecl] = {
      case Block(Nil, Function(vparams, rhs)) =>
        val params = listToExpr(vparams.map(v => q"${v.name.decodedName.toString}"))
        val body = jsFunBody(rhs)
        q"JsAnonFunDecl($params, $body)"
      case Function(vparams, rhs) =>
        val params = listToExpr(vparams.map(v => q"${v.name.decodedName.toString}"))
        val body = jsFunBody(rhs)
        q"JsAnonFunDecl($params, $body)"
    }

    lazy val jsTry: ToExpr[JsTry] = {
      case Try(body, catchBlock, finBody) =>
        val ctch = catchBlock match {
          case Nil => q"None"
          case List(CaseDef(Bind(pat, _), EmptyTree, catchBody)) =>
            q"Some(JsCatch(JsIdent(${pat.decodedName.toString}), ${jsStmtOrDie(catchBody)}))"
        }
        val fin = if (finBody.equalsStructure(EmptyTree)) q"None"
        else q"Some(${jsStmtOrDie(finBody)})"
        q"JsTry(${jsStmtOrDie(body)}, $ctch, $fin)"
    }

    lazy val jsThrowExpr: ToExpr[JsThrow] = {
      case Throw(expr) => q"JsThrow(${jsExprOrDie(expr)})"
    }

    def jsSwitchGen(expr: Tree, cases: List[CaseDef], f: Tree => Tree) = {
      val cs = cases collect {
        case CaseDef(const@Literal(Constant(_)), EmptyTree, body) => q"JsCase(List(${jsLit(const)}), ${jsStmtOrDie(f(body))})"
        case CaseDef(sel@Select(path, n), EmptyTree, body) =>
          q"JsCase(List(${jsExprOrDie(sel)}), ${jsStmtOrDie(f(body))})"
        case CaseDef(Alternative(xs), EmptyTree, body) =>
          val stmt = jsStmtOrDie(f(body))
          val consts = listToExpr(xs map(c => jsLit(c)))
          q"JsCase($consts, $stmt)"
      }
      val df = (cases collect {
        case CaseDef(Ident(nme.WILDCARD), EmptyTree, body) => q"Some(JsDefault(${jsStmtOrDie(f(body))}))"
      }).headOption.getOrElse(q"None")
      val css = listToExpr(cs)
      q"JsSwitch(${jsExprOrDie(expr)}, $css, $df)"
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
        q"JsUnit"
      case cd@ClassDef(_, clsName, _, t@Template(base, _, body)) =>
        val ctor = body.collect {
          case f@DefDef(mods, n, _, argss, _, Block(stats, _)) if n == nme.CONSTRUCTOR =>
            val a = argss.flatten.map(v => v.name.decodedName.toString)
            a
        }
        if (ctor.size != 1) c.abort(c.enclosingPosition, "Only single primary constructor is currently supported. Sorry.")
        val init = ctor.head.map(f => f -> q"JsIdent($f)")
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
        val ctorFuncDecl = q"JsFunDecl(${clsName.decodedName.toString}, $args, JsBlock($ctorBody))"
        q"JsObjDecl(${clsName.decodedName.toString}, $ctorFuncDecl, $fs)"
    }

    lazy val jsAnonObjDecl: ToExpr[JsAnonObjDecl] = {
      case Block(List(ClassDef(_, clsName, _, Template(_, _, body))), _/* Constructor call */) =>
        val defs = body.collect(objectFields)
        val params = listToExpr(defs.map { case (n, v) => q"($n, $v)" })
        q"JsAnonObjDecl($params)"
    }

    lazy val jsReturn1: ToExpr[JsStmt] = {
      case Return(expr) => q"JsReturn(${jsExprOrDie(expr)})"
    }
    lazy val jsReturn: ToExpr[JsStmt] = jsReturnStmt orElse jsStmtOrDie
    lazy val jsReturnStmt: ToExpr[JsReturn] = jsExpr andThen (jsExpr => q"JsReturn($jsExpr)")

    lazy val jsBlock: ToExpr[JsBlock] = {
      case Block(stmts, expr) =>
        val stmtTrees = if (expr.equalsStructure(q"()")) stmts else stmts :+ expr
        val ss = listToExpr(stmtTrees map jsStmtOrDie)
        q"JsBlock($ss)"
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
      q"${expr}.asInstanceOf[JsAst with ${tree.tpe}]"
    } else expr

    resultTree
  }
}
