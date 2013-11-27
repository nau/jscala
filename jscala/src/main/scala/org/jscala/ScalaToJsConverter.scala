package org.jscala

import language.implicitConversions
import language.experimental.macros
import scala.reflect.macros.Context
import scala.collection.generic.{SeqFactory, MapFactory}
import scala.reflect.internal.Flags

class ScalaToJsConverter[C <: Context](val c: C, debug: Boolean) extends JsBasis[C] {
  import c.universe._

  private val traits = collection.mutable.HashMap[String, List[Tree]]()

  def convert(tree: Tree): c.Expr[JsAst] = {
    //      println((tree))
    if (debug) println(tree.raw)

    lazy val jsSelect: ToExpr[JsExpr] = {
      case Select(Select(Select(Ident(Name("org")), Name("jscala")), Name("package")), Name(name)) =>
        reify(JsIdent(c.literal(name).splice))
      case Select(Select(Ident(Name("org")), Name("jscala")), Name(name)) =>
        reify(JsIdent(c.literal(name).splice))
      case Select(q, name) =>
        reify(JsSelect(jsExprOrDie(q).splice, c.literal(name.decoded).splice))
    }

    lazy val jsUnaryOp: ToExpr[JsUnOp] = {
      case Select(q, n) if encodedUnaryOpsMap.contains(n) =>
        val op = encodedUnaryOpsMap(n)
        reify(JsUnOp(c.literal(op).splice, jsExprOrDie(q).splice))
    }

    def funParams(args: List[Tree]): Expr[List[JsExpr]] = {
      val filteredDefaults = args collect {
        case arg@Select(_, n) if n.decoded.contains("$default$") => None
        case arg@Ident(n) if n.decoded.contains("$default$") => None
        case Typed(exp, _) => Some(jsExprOrDie(exp))
        case arg => Some(jsExprOrDie(arg))
      }
      listToExpr(filteredDefaults.flatten)
    }

    lazy val jsBinOp: ToExpr[JsBinOp] = {
      case Apply(Select(q, n), List(rhs)) if encodedBinOpsMap.contains(n) =>
        val op = encodedBinOpsMap(n)
        val opExpr = c.literal(op)
        val qExpr = jsExprOrDie(q)
        val rhsExpr = jsExprOrDie(rhs)
        // generate correct whole number devision JavaScript if a and b are [Byte,Short,Int,Long]: a/b|0
        if (op == "/" && q.isNum && rhs.isNum)
          reify(JsBinOp("|", JsBinOp(opExpr.splice, qExpr.splice, rhsExpr.splice), JsNum(0, false)))
        else reify(JsBinOp(opExpr.splice, qExpr.splice, rhsExpr.splice))
      case Assign(lhs, rhs) => reify(JsBinOp("=", jsExprOrDie(lhs).splice, jsExprOrDie(rhs).splice))
    }

    lazy val jsTupleExpr: PFT[(Tree, Tree)] = {
      case Apply(TypeApply(Select(Apply(TypeApply(path, _), List(lhs)), arrow), _), List(rhs))
        if path.is("scala.Predef.any2ArrowAssoc") && (arrow.decoded == "->" || arrow.decoded == "→") =>
        lhs -> rhs
      case Apply(TypeApply(path, _), List(lhs, rhs)) if path.is("scala.Tuple2.apply") => lhs -> rhs
    }

    def genMap(args: List[Tree]) = {
      val fields = for (arg <- args) yield {
        val (lhs, rhs) = jsTupleExpr(arg)
        jsString.applyOrElse(lhs, (t: Tree) => c.abort(arg.pos, "Map key type can only be String")) -> jsExprOrDie(rhs)
      }
      val params = listToExpr(fields.map { case (n, v) => reify((c.literal(n).splice, v.splice)) })
      reify(JsAnonObjDecl(params.splice))
    }

    lazy val jsMapExpr: ToExpr[JsExpr] = {
      case Apply(TypeApply(Select(path, Name("apply")), _), args) if path.tpe.baseClasses.contains(mapFactorySym) =>
        genMap(args)
      case Apply(Select(path, Name("apply")), List(index)) if path.tpe.baseClasses.contains(mapSym) =>
        reify(JsAccess(jsExprOrDie(path).splice, jsExprOrDie(index).splice))
      case Apply(Select(path, Name("update")), List(key, value)) if path.tpe.baseClasses.contains(mapSym) =>
        reify(JsBinOp("=", JsAccess(jsExprOrDie(path).splice, jsExprOrDie(key).splice), jsExprOrDie(value).splice))
    }

    lazy val jsForStmt: ToExpr[JsStmt] = {
      /*
        for (index <- from until untilExpr) body
        for (index <- from to untilExpr) body
      */
      case Apply(TypeApply(Select(Apply(Select(Apply(fn, List(Literal(Constant(from: Int)))), n@Name("until"|"to")), List(endExpr)), Name("foreach")), _),
      List(Function(List(ValDef(_, Name(index), _, _)), body))) if fn.is("scala.Predef.intWrapper") =>
        val forBody = jsStmtOrDie(body)
        val init = reify(varDef(c.literal(index).splice, JsNum(c.literal(from).splice, false)))
        val check = if (n.decoded == "until")
            reify(JsBinOp("<", JsIdent(c.literal(index).splice), jsExprOrDie(endExpr).splice))
          else reify(JsBinOp("<=", JsIdent(c.literal(index).splice), jsExprOrDie(endExpr).splice))
        val update = reify(JsUnOp("++", JsIdent(c.literal(index).splice)).stmt)
        reify(JsFor(List(init.splice), check.splice, List(update.splice), forBody.splice))
      /*
        val coll = Array(1, 2)
        for (ident <- coll) body
      */
      case Apply(TypeApply(Select(Apply(TypeApply(path, _), List(Ident(Name(coll)))), Name("foreach")), _), List(Function(List(ValDef(_, Name(ident), _, _)), body)))
        if path.is("scala.Predef.refArrayOps") => forStmt(ident, coll, body)
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
        reify(JsForIn(JsIdent(c.literal(ident).splice), jsExprOrDie(coll).splice, jsStmtOrDie(body).splice))
    }

    def forStmt(ident: String, coll: String, body: Tree) = {
      val idx = c.literal(ident + "Idx")
      val seq = reify(JsIdent(c.literal(coll).splice))
      val len = reify(JsSelect(seq.splice, "length"))
      val init = reify(JsVarDef(List(idx.splice -> JsNum(0, false), c.literal(ident).splice -> JsAccess(seq.splice, JsIdent(idx.splice)))))
      val check = reify(JsBinOp("<", JsIdent(idx.splice), len.splice))
      val update = reify(JsBinOp("=", JsIdent(c.literal(ident).splice), JsAccess(JsIdent(c.literal(coll).splice), JsUnOp("++", JsIdent(idx.splice)))).stmt)
      val forBody = jsStmtOrDie(body)
      reify(JsFor(List(init.splice), check.splice, List(update.splice), forBody.splice))
    }

    lazy val jsArrayExpr: ToExpr[JsExpr] = {
      // Array creation
      case Apply(TypeApply(path, _), args) if path.is("org.jscala.JArray.apply") =>
        val params = listToExpr(args map jsExprOrDie)
        reify(JsArray(params.splice))
      case TypeApply(path, args) if path.is("scala.Array.apply") =>
        val params = listToExpr(args map jsExprOrDie)
        reify(JsArray(params.splice))
      case Apply(Apply(TypeApply(path, _), args), _) if path.is("scala.Array.apply") =>
        val params = listToExpr(args map jsExprOrDie)
        reify(JsArray(params.splice))
      case TypeApply(path, args) if path.is("scala.Array.apply") =>
        val params = listToExpr(args map jsExprOrDie)
        reify(JsArray(params.splice))
      case Apply(path, args) if path.is("scala.Array.apply") =>
        val params = listToExpr(args map jsExprOrDie)
        reify(JsArray(params.splice))
      case Apply(Ident(Name("Array")), args) =>
        val params = listToExpr(args map jsExprOrDie)
        reify(JsArray(params.splice))
      case Apply(Select(New(AppliedTypeTree(Ident(arr), _)), ctor), List(Literal(Constant(_)))) if ctor == nme.CONSTRUCTOR && arr == newTypeName("Array") =>
        reify(JsArray(Nil))
      // new Array[Int](256)
      case Apply(Select(a@New(t@TypeTree()), ctor), List(Literal(Constant(_))))
        if ctor == nme.CONSTRUCTOR && t.original.isInstanceOf[AppliedTypeTree @unchecked] && t.original.asInstanceOf[AppliedTypeTree].tpt.equalsStructure(Select(Ident(newTermName("scala")), newTypeName("Array"))) =>
        reify(JsArray(Nil))
      case Apply(Select(a@New(t@TypeTree()), ctor), List(Literal(Constant(_)))) =>
        reify(JsArray(Nil))
      case Apply(TypeApply(Select(path, Name("apply")), _), args) if path.tpe.baseClasses.contains(seqFactorySym) =>
        val params = listToExpr(args map jsExprOrDie)
        reify(JsArray(params.splice))
      // Array access
      case Apply(Select(path, Name("apply")), List(idx)) if isArray(path) =>
        reify(JsAccess(jsExprOrDie(path).splice, jsExprOrDie(idx).splice))
      // Array update
      case Apply(Select(path, Name("update")), List(key, value)) if isArray(path) =>
        reify(JsBinOp("=", JsAccess(jsExprOrDie(path).splice, jsExprOrDie(key).splice), jsExprOrDie(value).splice))
    }

    lazy val jsGlobalFuncsExpr: ToExpr[JsExpr] = {
      case Select(Apply(path, List(arg)), Name("jstr")) => reify(jsExprOrDie(arg).splice)
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
        reify(JsRaw(c.literal(js).splice))
      case app@Apply(Select(path, _), List(ident)) if path.is("org.jscala.package.inject") =>
        reify(JsLazy(() => jsAst(ident).splice))
      case app@Apply(Apply(TypeApply(path, _), List(ident)), List(jss)) if path.is("org.jscala.package.inject") =>
        val call = c.Expr[JsExpr](Apply(jss, List(ident)))
        reify(JsLazy(() => call.splice))
      case Apply(Select(path, fn), args) if path.is("org.jscala.package") =>
        val params = funParams(args)
        reify(JsCall(JsIdent(c.literal(fn.decoded).splice), params.splice))
    }

    lazy val jsNewExpr: ToExpr[JsExpr] = {
      case Apply(Select(New(Ident(ident)), _), args) =>
        val params = funParams(args)
        reify(JsNew(JsCall(JsIdent(c.literal(ident.decoded).splice), params.splice)))
      case Apply(Select(New(path), _), args) =>
        val params = funParams(args)
        reify(JsNew(JsCall(jsExprOrDie(path).splice, params.splice)))
    }

    lazy val jsCallExpr: ToExpr[JsExpr] = {
      case Apply(Select(lhs, name), List(rhs)) if name.decoded.endsWith("_=") =>
        reify(JsBinOp("=", JsSelect(jsExprOrDie(lhs).splice, c.literal(name.decoded.dropRight(2)).splice), jsExprOrDie(rhs).splice))
      case Apply(Apply(Select(sel, Name("applyDynamic")), List(Literal(Constant(name: String)))), args) =>
        val callee = reify(JsSelect(jsExprOrDie(sel).splice, c.literal(name).splice))
        val params = listToExpr(args.map(jsExprOrDie))
        reify(JsCall(callee.splice, params.splice))
      case Apply(Apply(Select(sel, Name("updateDynamic")), List(Literal(Constant(name: String)))), List(arg)) =>
        val callee = reify(JsSelect(jsExprOrDie(sel).splice, c.literal(name).splice))
        reify(JsBinOp("=", callee.splice, jsExprOrDie(arg).splice))
      case Apply(Select(sel, Name("selectDynamic")), List(Literal(Constant(name: String)))) =>
        reify(JsSelect(jsExprOrDie(sel).splice, c.literal(name).splice))
      case app@Apply(fun, args) if app.tpe <:< typeOf[JsAst] =>
        val expr = c.Expr[JsAst](app)
        reify(JsLazy(() => expr.splice))
      case Apply(Select(p@Ident(_), Name(fun)), args) if p.symbol.isModule =>
        val params = funParams(args)
        reify(JsCall(JsIdent(c.literal(fun).splice), params.splice))
      case Apply(fun, args) =>
        val callee = jsExprOrDie apply fun
        val params = funParams(args)
        reify(JsCall(callee.splice, params.splice))
    }

    lazy val jsIfStmt: ToExpr[JsIf] = {
      case If(cond, thenp, elsep) =>
        val condJsExpr = jsExprOrDie(cond)
        val thenJsExpr = jsStmtOrDie(thenp)
        val elseJsStmt = if (isUnit(elsep)) reify(None) else reify(Some(jsStmtOrDie(elsep).splice))
        reify(JsIf(condJsExpr.splice, thenJsExpr.splice, elseJsStmt.splice))
    }

    lazy val jsTernaryExpr: ToExpr[JsTernary] = {
      case If(cond, thenp, elsep) if !thenp.tpe.=:=(typeOf[Unit]) && !isUnit(elsep) && jsExpr.isDefinedAt(thenp) && jsExpr.isDefinedAt(elsep) =>
        val condJsExpr = jsExprOrDie(cond)
        val thenJsExpr = jsExprOrDie(thenp)
        val elseExpr = reify(jsExprOrDie(elsep).splice)
        reify(JsTernary(condJsExpr.splice, thenJsExpr.splice, elseExpr.splice))
    }

    lazy val jsWhileStmt: ToExpr[JsWhile] = {
      case LabelDef(termName, Nil, If(cond, Block(List(body), _), _)) if termName.encoded.startsWith("while$") =>
        val condJsExpr = jsExprOrDie(cond)
        val bodyJsStmt = jsStmtOrDie(body)
        reify(JsWhile(condJsExpr.splice, bodyJsStmt.splice))
    }

    def addAssign(tree: Tree, name: Name) = tree match {
      case Block(stats, expr) => Block(stats :+ Assign(Ident(name), expr), c.literalUnit.tree)
      case expr => Block(Assign(Ident(name), expr) :: Nil, c.literalUnit.tree)
    }

    lazy val jsIfExpr: PartialFunction[(Name, Tree), Expr[JsIf]] = {
      case (name, If(cond, thenp, elsep)) =>
        val condJsExpr = jsExprOrDie(cond)
        val thenJsExpr = jsStmtOrDie(addAssign(thenp, name))
        val elseJsExpr = jsStmtOrDie(addAssign(elsep, name))
        reify(JsIf(condJsExpr.splice, thenJsExpr.splice, Some(elseJsExpr.splice)))
    }

    lazy val jsMatchExpr: PartialFunction[(Name, Tree), Expr[JsSwitch]] = {
      case (name, Match(expr, cases)) => jsSwitchGen(expr, cases, body => addAssign(body, name))
    }

    lazy val jsVarDefStmt: ToExpr[JsStmt] = {
      case ValDef(_, name, _, rhs) =>
        val identifier = c.literal(name.decoded)
        if (jsTernaryExpr.isDefinedAt(rhs)) {
          val idents = listToExpr(List(reify(identifier.splice -> jsTernaryExpr(rhs).splice)))
          reify(JsVarDef(idents.splice))
        } else {
          val funcs = Seq(jsIfExpr, jsMatchExpr).reduceLeft(_ orElse _) andThen { expr =>
            val ident = listToExpr(List(reify(identifier.splice -> JsUnit)))
            reify(JsStmts(List(JsVarDef(ident.splice), expr.splice)))
          }
          val x = name -> rhs
          funcs.applyOrElse(x, (t: (TermName, Tree)) => {
            val ident = listToExpr(List(reify(identifier.splice -> jsExprOrDie(rhs).splice)))
            reify(JsVarDef(ident.splice))
          })
        }

    }

    lazy val jsFunBody: ToExpr[JsBlock] = {
      case lit@Literal(_) =>
        val body = if (isUnit(lit)) Nil else List(jsReturnStmt(lit))
        reify(JsBlock(listToExpr(body).splice))
      case b@Block(stmts, expr) =>
        val lastExpr = if (isUnit(expr)) Nil
        else if (expr.tpe =:= typeOf[Unit]) List(jsStmtOrDie(expr))
        else List(jsReturn(expr))
        val ss = listToExpr(stmts.map(jsStmtOrDie) ::: lastExpr)
        reify(JsBlock(ss.splice))
      case rhs =>
        if (rhs.tpe =:= typeOf[Unit]) reify(JsBlock(List(jsStmtOrDie(rhs).splice)))
        else reify(JsBlock(List(jsReturn(rhs).splice)))
    }

    lazy val jsFunDecl: ToExpr[JsFunDecl] = {
      case DefDef(_, name, _, vparamss, _, rhs) =>
        val ident = c.literal(name.decoded)
        val a = vparamss.headOption.map(vp => vp.map(v => c.literal(v.name.decoded))).getOrElse(Nil)
        val params = listToExpr(a)
        val body = jsFunBody(rhs)
        reify(JsFunDecl(ident.splice, params.splice, body.splice))
    }

    lazy val jsAnonFunDecl: ToExpr[JsAnonFunDecl] = {
      case Block(Nil, Function(vparams, rhs)) =>
        val params = listToExpr(vparams.map(v => c.literal(v.name.decoded)))
        val body = jsFunBody(rhs)
        reify(JsAnonFunDecl(params.splice, body.splice))
      case Function(vparams, rhs) =>
        val params = listToExpr(vparams.map(v => c.literal(v.name.decoded)))
        val body = jsFunBody(rhs)
        reify(JsAnonFunDecl(params.splice, body.splice))
    }

    lazy val jsTry: ToExpr[JsTry] = {
      case Try(body, catchBlock, finBody) =>
        val ctch = catchBlock match {
          case Nil => reify(None)
          case List(CaseDef(Bind(pat, _), EmptyTree, catchBody)) =>
            reify(Some(JsCatch(JsIdent(c.literal(pat.decoded).splice), jsStmtOrDie(catchBody).splice)))
        }
        val fin = if (finBody.equalsStructure(EmptyTree)) reify(None)
        else reify(Some(jsStmtOrDie(finBody).splice))
        reify(JsTry(jsStmtOrDie(body).splice, ctch.splice, fin.splice))
    }

    lazy val jsThrowExpr: ToExpr[JsThrow] = {
      case Throw(expr) => reify(JsThrow(jsExprOrDie(expr).splice))
    }

    def jsSwitchGen(expr: Tree, cases: List[CaseDef], f: Tree => Tree) = {
      val cs = cases collect {
        case CaseDef(const@Literal(Constant(_)), EmptyTree, body) => reify(JsCase(List(jsLit(const).splice), jsStmtOrDie(f(body)).splice))
        case CaseDef(sel@Select(path, n), EmptyTree, body) => reify(JsCase(List(jsExprOrDie(sel).splice), jsStmtOrDie(f(body)).splice))
        case CaseDef(Alternative(xs), EmptyTree, body) =>
          val stmt = jsStmtOrDie(f(body))
          val consts = listToExpr(xs map(c => jsLit(c)))
          reify(JsCase(consts.splice, stmt.splice))
      }
      val df = (cases collect {
        case CaseDef(Ident(nme.WILDCARD), EmptyTree, body) => reify(Some(JsDefault(jsStmtOrDie(f(body)).splice)))
      }).headOption.getOrElse(reify(None))
      val css = listToExpr(cs)
      reify(JsSwitch(jsExprOrDie(expr).splice, css.splice, df.splice))
    }

    lazy val jsSwitch: ToExpr[JsSwitch] = {
      case Match(expr, cases) => jsSwitchGen(expr, cases, t => t)
    }

    def eligibleDef(f: DefDef) = {
      f.name != nme.CONSTRUCTOR && f.name.decoded != "$init$" && !f.mods.hasFlag(Flags.ACCESSOR.toLong.asInstanceOf[FlagSet] | Flag.DEFERRED)
    }

    lazy val objectFields: PFT[(String, Expr[JsExpr])] = {
      case f@DefDef(mods, n, _, argss, _, body) if eligibleDef(f) =>
        n.decoded -> jsExprOrDie(Function(argss.flatten, body))
      case ValDef(mods, n, _, rhs) if !rhs.equalsStructure(EmptyTree)
        && !mods.hasFlag(Flags.PARAMACCESSOR.toLong.asInstanceOf[FlagSet] | Flag.ABSTRACT) => n.decoded.trim -> jsExprOrDie(rhs)
    }

    lazy val jsClassDecl: ToExpr[JsStmt] = {
      case cd@ClassDef(mods, clsName, _, Template(_, _, body)) if mods.hasFlag(Flag.TRAIT) =>
        // Remember trait AST to embed its definitions in concrete class for simplicity
        traits(cd.symbol.fullName) = body
        reify(JsExprStmt(JsUnit))
      case cd@ClassDef(_, clsName, _, t@Template(base, _, body)) =>
        val ctor = body.collect {
          case f@DefDef(mods, n, _, argss, _, Block(stats, _)) if n == nme.CONSTRUCTOR =>
            val a = argss.flatten.map(v => v.name.decoded)
            a
        }
        if (ctor.size != 1) c.abort(c.enclosingPosition, "Only single primary constructor is currently supported. Sorry.")
        val init = ctor.head.map(f => f -> reify(JsIdent(c.literal(f).splice)))
        val inherited = t.tpe.baseClasses.map(_.fullName).flatMap {bc => traits.get(bc).toList }
        val bigBody = inherited.foldRight(List[Tree]())(_ ::: _) ::: body
        val defs = bigBody.collect(objectFields)
        val fields = init ::: defs
        val fs = listToExpr(fields.map { case (n, v) => reify((c.literal(n).splice, v.splice)) })
        val args = listToExpr(ctor.head.map(arg => c.literal(arg)))
        val ctorBody = listToExpr(bigBody.collect {
          case _: ValDef => None
          case _: DefDef => None
          case stmt => Some(stmt)
        }.flatten.map(jsStmtOrDie))
        val ctorFuncDecl = reify(JsFunDecl(c.literal(clsName.decoded).splice, args.splice, JsBlock(ctorBody.splice)))
        reify(JsObjDecl(c.literal(clsName.decoded).splice, ctorFuncDecl.splice, fs.splice))
    }

    lazy val jsAnonObjDecl: ToExpr[JsAnonObjDecl] = {
      case Block(List(ClassDef(_, clsName, _, Template(_, _, body))), _/* Constructor call */) =>
        val defs = body.collect(objectFields)
        val params = listToExpr(defs.map { case (n, v) => reify((c.literal(n).splice, v.splice)) })
        reify(JsAnonObjDecl(params.splice))
    }

    lazy val jsReturn1: ToExpr[JsStmt] = {
      case Return(expr) => reify(JsReturn(jsExprOrDie(expr).splice))
    }
    lazy val jsReturn: ToExpr[JsStmt] = jsReturnStmt orElse jsStmtOrDie
    lazy val jsReturnStmt: ToExpr[JsReturn] = jsExpr andThen (jsExpr => reify(JsReturn(jsExpr.splice)))

    lazy val jsBlock: ToExpr[JsBlock] = {
      case Block(stmts, expr) =>
        val stmtTrees = if (expr.equalsStructure(c.literalUnit.tree)) stmts else stmts :+ expr
        val ss = listToExpr(stmtTrees map jsStmtOrDie)
        reify(JsBlock(ss.splice))
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
    def jsExprOrError(msg: String): ToExpr[JsExpr] = jsExpr orElse die(msg)

    lazy val jsExprStmt: ToExpr[JsExprStmt] = jsExpr andThen (jsExpr => reify(JsExprStmt(jsExpr.splice)))

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
      jsExprStmt
    ) reduceLeft (_ orElse _)

    lazy val jsStmtOrDie: ToExpr[JsStmt] = jsStmt orElse die("Unsupported syntax")

    lazy val jsAst: ToExpr[JsAst] = jsBlock orElse jsExpr orElse jsStmtOrDie

    val expr = jsAst apply tree

    if (functionTypes.exists(tree.tpe.<:<)) {
      c.Expr(q"${expr.tree}.asInstanceOf[${expr.staticType} with ${tree.tpe}]")
    } else expr
  }
}
