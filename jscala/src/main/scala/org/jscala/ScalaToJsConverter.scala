package org.jscala

import language.implicitConversions
import language.experimental.macros
import scala.reflect.macros.Context
import scala.collection.generic.{SeqFactory, MapFactory}
import scala.reflect.internal.Flags

class ScalaToJsConverter[C <: Context](val c: C) {
  import c.universe._
  type PFT[A] = PartialFunction[Tree, A]
  type ToExpr[A] = PFT[Expr[A]]

  private val unaryOps = Seq("+", "-", "!")
  private val encodedUnaryOpsMap = unaryOps.map(op => newTermName(s"unary_$op").encodedName -> op).toMap
  private val binOps = Seq("*", "/", "%",  "+", "-", "<<", ">>", ">>>",
    "<", ">", "<=", ">=",
    "==", "!=", "&", "|", "&&", "||")
  private val encodedBinOpsMap = binOps.map(op => newTermName(op).encodedName -> op).toMap
  private lazy val seqFactorySym = c.typeOf[SeqFactory[Seq]].typeSymbol
  private lazy val mapFactorySym = c.typeOf[MapFactory[collection.Map]].typeSymbol
  private lazy val jarraySym = c.mirror.staticClass("org.jscala.JArray")
  private lazy val seqSym = c.mirror.staticClass("scala.collection.Seq")
  private lazy val mapSym = c.mirror.staticClass("scala.collection.Map")
  private lazy val jsAstSym = c.typeOf[JsAst].typeSymbol

  implicit class TreeHelper(tree: Tree) {
    def is(p: String) = tree.equalsStructure(select(p)) || tree.equalsStructure(select(p, s => This(newTypeName(s))))
  }

  object Name {
    def unapply(name: Name) = Some(name.decoded)
  }

  private def select(p: String, init: String => Tree = s => Ident(newTermName(s))): Tree = {
    p.split("\\.").foldLeft(EmptyTree) {
      case (EmptyTree, el) => init(el)
      case (t, el) => Select(t, newTermName(el))
    }
  }
  private def isUnit(tree: Tree) = tree.equalsStructure(c.literalUnit.tree)
  private def isNull(tree: Tree) = tree.equalsStructure(c.literalNull.tree)
  private def isArray(path: c.Tree) =
    path.tpe.typeSymbol == definitions.ArrayClass || path.tpe.typeSymbol == jarraySym || path.tpe.baseClasses.contains(seqSym)
  private def listToExpr[T](exprs: List[Expr[T]]): Expr[List[T]] = c.Expr[List[T]](treeBuild.mkMethodCall(reify(List).tree, exprs.map(_.tree)))
  private def mapToExpr[V](m: Map[String, Expr[V]]): Expr[Map[String, V]] = {
    val args: List[Expr[(String, V)]] =  m.map { case (k, v) => reify(c.literal(k).splice -> v.splice) }.toList
    val params =  args.map(_.tree)
    c.Expr[Map[String, V]](treeBuild.mkMethodCall(reify(Map).tree, params))
  }

  private lazy val jsString: PFT[String] = {
    case Literal(Constant(value: Char))  => value.toString
    case Literal(Constant(value: String))  => value
  }
  private lazy val jsStringLit: ToExpr[JsString] = jsString.andThen(s => reify(JsString(c.literal(s).splice)))

  private lazy val jsNumLit: ToExpr[JsNum] = {
    case Literal(Constant(value: Byte))  => reify(JsNum(c.literal(value).splice, isFloat = false))
    case Literal(Constant(value: Short))  => reify(JsNum(c.literal(value).splice, isFloat = false))
    case Literal(Constant(value: Int))  => reify(JsNum(c.literal(value).splice, isFloat = false))
    case Literal(Constant(value: Long))  => reify(JsNum(c.literal(value).splice, isFloat = false))
    case Literal(Constant(value: Double))  => reify(JsNum(c.literal(value).splice, isFloat = true))
  }
  private lazy val jsBoolLit: ToExpr[JsBool] = {
    case Literal(Constant(value: Boolean))  => reify(JsBool(c.literal(value).splice))
  }
  private object jsUnitLit extends PartialFunction[Tree, Expr[JsUnit.type]] {
    def apply(v1: Tree) = reify(JsUnit)
    def isDefinedAt(x: Tree) = isUnit(x)
  }
  private object jsNullLit extends PartialFunction[Tree, Expr[JsNull.type]] {
    def apply(v1: Tree) = reify(JsNull)
    def isDefinedAt(x: Tree) = isNull(x)
  }

  private val jsLit: ToExpr[JsLit] = {
    jsStringLit orElse jsNumLit orElse jsBoolLit orElse jsNullLit orElse jsUnitLit
  }

  def convert(tree: Tree): c.Expr[JsAst] = {
    //      println((tree))
//          println(showRaw(tree))

    lazy val jsThis: ToExpr[JsIdent] = {
      case This(name) => reify(JsIdent("this"))
    }

    lazy val jsIdent: ToExpr[JsIdent] = {
      case Ident(name) => reify(JsIdent(c.literal(name.decoded).splice))
    }

    lazy val jsSelect: ToExpr[JsExpr] = {
      case Select(Select(Select(Ident(Name("org")), Name("jscala")), Name("package")), Name(name)) =>
        reify(JsIdent(c.literal(name).splice))
      case Select(Select(Ident(Name("org")), Name("jscala")), Name(name)) =>
        reify(JsIdent(c.literal(name).splice))
      case Select(q, name) =>
        reify(JsSelect(jsExpr(q).splice, c.literal(name.decoded).splice))
    }

    lazy val jsUnaryOp: ToExpr[JsUnOp] = {
      case Select(q, n) if encodedUnaryOpsMap.contains(n) =>
        val op = encodedUnaryOpsMap(n)
        reify(JsUnOp(c.literal(op).splice, jsExpr(q).splice))
    }

    lazy val jsBinOp: ToExpr[JsBinOp] = {
      case Apply(Select(q, n), List(rhs)) if encodedBinOpsMap.contains(n) =>
        val op = encodedBinOpsMap(n)
        reify(JsBinOp(c.literal(op).splice, jsExpr(q).splice, jsExpr(rhs).splice))
      case Assign(lhs, rhs) => reify(JsBinOp("=", jsExpr(lhs).splice, jsExpr(rhs).splice))
    }

    lazy val jsTupleExpr: PFT[(Tree, Tree)] = {
      case Apply(TypeApply(Select(Apply(TypeApply(path, _), List(lhs)), arrow), _), List(rhs))
        if path.is("scala.Predef.any2ArrowAssoc") && (arrow.decoded == "->" || arrow.decoded == "→") =>
        lhs -> rhs
      case Apply(TypeApply(path, _), List(lhs, rhs)) if path.is("scala.Tuple2.apply") => lhs -> rhs
    }

    def genMap(args: List[Tree]) = {
      val map = for (arg <- args) yield {
        val (lhs, rhs) = jsTupleExpr(arg)
        jsString.applyOrElse(lhs, (t: Tree) => c.abort(arg.pos, "Map key type can only be String")) -> jsExpr(rhs)
      }
      val params = mapToExpr(map.toMap)
      reify(JsAnonObjDecl(params.splice))
    }

    lazy val jsMapExpr: ToExpr[JsExpr] = {
      case Apply(TypeApply(Select(path, Name("apply")), _), args) if path.tpe.baseClasses.contains(mapFactorySym) =>
        genMap(args)
      case Apply(Select(path, Name("apply")), List(index)) if path.tpe.baseClasses.contains(mapSym) =>
        reify(JsAccess(jsExpr(path).splice, jsExpr(index).splice))
      case Apply(Select(path, Name("update")), List(key, value)) if path.tpe.baseClasses.contains(mapSym) =>
        reify(JsBinOp("=", JsAccess(jsExpr(path).splice, jsExpr(key).splice), jsExpr(value).splice))
    }

    lazy val jsForStmt: ToExpr[JsStmt] = {
      case Apply(TypeApply(Select(Apply(Select(Apply(fn, List(Literal(Constant(from: Int)))), Name("until")), List(untilExpr)), Name("foreach")), _),
      List(Function(List(ValDef(_, index, _, _)), body))) if fn.is("scala.Predef.intWrapper") =>
        val forBody = jsStmt(body)
        reify(JsFor(JsIdent(c.literal(index.decoded).splice), JsNum(c.literal(from).splice, false), jsExpr(untilExpr).splice, forBody.splice))
      case Apply(TypeApply(Select(Apply(TypeApply(path, _), List(Ident(coll))), Name("foreach")), _), List(Function(List(ValDef(_, ident, _, _)), body)))
        if path.is("scala.Predef.refArrayOps") =>
        val forBody = jsStmt(body)
        reify(JsForIn(JsIdent(c.literal(coll.decoded).splice), JsIdent(c.literal(ident.decoded).splice), forBody.splice))
    }

    lazy val jsArrayExpr: ToExpr[JsExpr] = {
      // Array creation
      case Apply(TypeApply(path, _), args) if path.is("org.jscala.JArray.apply") =>
        val params = listToExpr(args map jsExpr)
        reify(JsArray(params.splice))
      case TypeApply(path, args) if path.is("scala.Array.apply") =>
        val params = listToExpr(args map jsExpr)
        reify(JsArray(params.splice))
      case Apply(Apply(TypeApply(path, _), args), _) if path.is("scala.Array.apply") =>
        val params = listToExpr(args map jsExpr)
        reify(JsArray(params.splice))
      case TypeApply(path, args) if path.is("scala.Array.apply") =>
        val params = listToExpr(args map jsExpr)
        reify(JsArray(params.splice))
      case Apply(path, args) if path.is("scala.Array.apply") =>
        val params = listToExpr(args map jsExpr)
        reify(JsArray(params.splice))
      case Apply(TypeApply(Select(path, Name("apply")), _), args) if path.tpe.baseClasses.contains(seqFactorySym) =>
        val params = listToExpr(args map jsExpr)
        reify(JsArray(params.splice))
      // Array access
      case Apply(Select(path, Name("apply")), List(idx)) if isArray(path) =>
        reify(JsAccess(jsExpr(path).splice, jsExpr(idx).splice))
      // Array update
      case Apply(Select(path, Name("update")), List(key, value)) if isArray(path) =>
        reify(JsBinOp("=", JsAccess(jsExpr(path).splice, jsExpr(key).splice), jsExpr(value).splice))
    }

    lazy val jsGlobalFuncsExpr: ToExpr[JsExpr] = {
      case TypeApply(Select(Apply(jsAnyOps, List(expr)), Name("as")), _) if jsAnyOps.is("org.jscala.package.JsAnyOps") =>
        jsExpr(expr)
      case Select(expr, Name("toDouble")) => jsExpr(expr)
      case TypeApply(Select(expr, Name("asInstanceOf")), _) =>
        jsExpr(expr)
      case Apply(TypeApply(Select(path, Name(n)), _), List(expr)) if path.is("org.jscala.package") && n.startsWith("implicit") =>
        jsExpr(expr)
      case Apply(Select(path, Name(n)), List(expr)) if path.is("org.jscala.package") && n.startsWith("implicit") =>
        jsExpr(expr)
      case Apply(path, List(Literal(Constant(js: String)))) if path.is("org.jscala.package.include") =>
        reify(JsRaw(c.literal(js).splice))
      case app@Apply(Select(path, _), List(ident)) if path.is("org.jscala.package.inject") =>
        reify(JsLazy(() => jsAst(ident).splice))
      case app@Apply(Apply(TypeApply(path, _), List(ident)), List(jss)) if path.is("org.jscala.package.inject") =>
        val call = c.Expr[JsExpr](Apply(jss, List(ident)))
        reify(JsLazy(() => call.splice))
      case Apply(Select(path, fn), args) if path.is("org.jscala.package") =>
        val params = listToExpr(args map jsExpr)
        reify(JsCall(JsIdent(c.literal(fn.decoded).splice), params.splice))
    }

    lazy val jsJStringExpr: ToExpr[JsExpr] = {
      case Apply(Select(New(Select(Select(Ident(Name("org")), Name("jscala")), Name("JString"))), _), List(Literal(Constant(str: String)))) =>
        reify(JsString(c.literal(str).splice))
    }

    lazy val jsNewExpr: ToExpr[JsExpr] = {
      case Apply(Select(New(Ident(ident)), _), args) =>
        val params = listToExpr(args map jsExpr)
        reify(JsNew(JsCall(JsIdent(c.literal(ident.decoded).splice), params.splice)))
      case Apply(Select(New(path), _), args) =>
        val params = listToExpr(args map jsExpr)
        reify(JsNew(JsCall(jsExpr(path).splice, params.splice)))
    }

    lazy val jsCallExpr: ToExpr[JsExpr] = {
      case Apply(Select(lhs, name), List(rhs)) if name.decoded.endsWith("_=") =>
        reify(JsBinOp("=", JsSelect(jsExpr(lhs).splice, c.literal(name.decoded.dropRight(2)).splice), jsExpr(rhs).splice))
      case Apply(Apply(Select(sel, Name("applyDynamic")), List(Literal(Constant(name: String)))), args) =>
        val callee = reify(JsSelect(jsExpr(sel).splice, c.literal(name).splice))
        val params = listToExpr(args.map(jsExpr))
        reify(JsCall(callee.splice, params.splice))
      case Apply(Apply(Select(sel, Name("updateDynamic")), List(Literal(Constant(name: String)))), List(arg)) =>
        val callee = reify(JsSelect(jsExpr(sel).splice, c.literal(name).splice))
        reify(JsBinOp("=", callee.splice, jsExpr(arg).splice))
      case Apply(Select(sel, Name("selectDynamic")), List(Literal(Constant(name: String)))) =>
        reify(JsSelect(jsExpr(sel).splice, c.literal(name).splice))
      case app@Apply(fun, args) if app.tpe <:< typeOf[JsAst] =>
        val expr = c.Expr[JsAst](app)
        reify(JsLazy(() => expr.splice))
      case Apply(fun, args) =>
        val callee = jsExpr apply fun
        val filteredDefaults = args collect {
          case arg@Select(_, n) if n.decoded.contains("$default$") => None
          case arg@Ident(n) if n.decoded.contains("$default$") => None
          case arg => Some(jsExpr(arg))
        }
        val params = listToExpr(filteredDefaults.flatten)
        reify(JsCall(callee.splice, params.splice))
    }

    lazy val jsIfStmt: ToExpr[JsIf] = {
      case If(cond, thenp, elsep) =>
        val condJsExpr = jsExpr(cond)
        val thenJsExpr = jsStmt(thenp)
        val elseJsStmt = if (isUnit(elsep)) reify(None) else reify(Some(jsStmt(elsep).splice))
        reify(JsIf(condJsExpr.splice, thenJsExpr.splice, elseJsStmt.splice))
    }

    lazy val jsWhileStmt: ToExpr[JsWhile] = {
      case LabelDef(termName, Nil, If(cond, Block(List(body), _), _)) if termName.encoded.startsWith("while$") =>
        val condJsExpr = jsExpr(cond)
        val bodyJsStmt = jsStmt(body)
        reify(JsWhile(condJsExpr.splice, bodyJsStmt.splice))
    }

    def addAssign(tree: Tree, name: Name) = tree match {
      case Block(stats, expr) => Block(stats :+ Assign(Ident(name), expr), c.literalUnit.tree)
      case expr => Block(Assign(Ident(name), expr) :: Nil, c.literalUnit.tree)
    }

    lazy val jsIfExpr: PartialFunction[(Name, Tree), Expr[JsIf]] = {
      case (name, If(cond, thenp, elsep)) =>
        val condJsExpr = jsExpr(cond)
        val thenJsExpr = jsStmt(addAssign(thenp, name))
        val elseJsExpr = jsStmt(addAssign(elsep, name))
        reify(JsIf(condJsExpr.splice, thenJsExpr.splice, Some(elseJsExpr.splice)))
    }

    lazy val jsMatchExpr: PartialFunction[(Name, Tree), Expr[JsSwitch]] = {
      case (name, Match(expr, cases)) => jsSwitchGen(expr, cases, body => addAssign(body, name))
    }

    lazy val jsVarDefStmt: ToExpr[JsStmt] = {
      case ValDef(_, name, _, rhs) =>
        val identifier = c.literal(name.decoded)
        val funcs = Seq(jsIfExpr, jsMatchExpr).reduceLeft(_ orElse _) andThen { expr =>
          reify(JsStmts(List(JsVarDef(identifier.splice, JsUnit), expr.splice)))
        }
        val x = name -> rhs
        funcs.applyOrElse(x, (t: (TermName, Tree)) => reify(JsVarDef(identifier.splice, jsExpr(rhs).splice)))
    }

    lazy val jsFunBody: ToExpr[JsBlock] = {
      case lit@Literal(_) =>
        val body = if (isUnit(lit)) Nil else List(jsReturnStmt(lit))
        reify(JsBlock(listToExpr(body).splice))
      case b@Block(stmts, expr) =>
        val lastExpr = if (isUnit(expr)) Nil
        else if (expr.tpe =:= typeOf[Unit]) List(jsStmt(expr))
        else List(jsReturn(expr))
        val ss = listToExpr(stmts.map(jsStmt) ::: lastExpr)
        reify(JsBlock(ss.splice))
      case rhs =>
        if (rhs.tpe =:= typeOf[Unit]) reify(JsBlock(List(jsStmt(rhs).splice)))
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
            reify(Some(JsCatch(JsIdent(c.literal(pat.decoded).splice), jsStmt(catchBody).splice)))
        }
        val fin = if (finBody.equalsStructure(EmptyTree)) reify(None)
        else reify(Some(jsStmt(finBody).splice))
        reify(JsTry(jsStmt(body).splice, ctch.splice, fin.splice))
    }

    def jsSwitchGen(expr: Tree, cases: List[CaseDef], f: Tree => Tree) = {
      val cs = cases collect {
        case CaseDef(const@Literal(Constant(_)), EmptyTree, body) => List(reify(JsCase(jsLit(const).splice, jsStmt(f(body)).splice)))
        case CaseDef(sel@Select(path, n), EmptyTree, body) => List(reify(JsCase(jsExpr(sel).splice, jsStmt(f(body)).splice)))
        case CaseDef(Alternative(xs), EmptyTree, body) =>
          val stmt = jsStmt(f(body))
          for (const <- xs) yield reify(JsCase(jsLit(const).splice, stmt.splice))
      }
      val df = (cases collect {
        case CaseDef(Ident(nme.WILDCARD), EmptyTree, body) => reify(Some(JsDefault(jsStmt(f(body)).splice)))
      }).headOption.getOrElse(reify(None))
      val css = listToExpr(cs.flatten)
      reify(JsSwitch(jsExpr(expr).splice, css.splice, df.splice))
    }

    lazy val jsSwitch: ToExpr[JsSwitch] = {
      case Match(expr, cases) => jsSwitchGen(expr, cases, t => t)
    }

    lazy val objectFields: PFT[(String, Expr[JsExpr])] = {
      case f@DefDef(mods, n, _, argss, _, body) if n != nme.CONSTRUCTOR && !mods.hasFlag(Flags.ACCESSOR.toLong.asInstanceOf[FlagSet]) => n.decoded -> jsExpr(Function(argss.flatten, body))
      case ValDef(mods, n, _, rhs) if !rhs.equalsStructure(EmptyTree) && !mods.hasFlag(Flags.PARAMACCESSOR.toLong.asInstanceOf[FlagSet]) => n.decoded.trim -> jsExpr(rhs)
    }

    lazy val jsClassDecl: ToExpr[JsObjDecl] = {
      case cd@ClassDef(_, clsName, _, Template(_, _, body)) =>
        val ctor = body.collect {
          case f@DefDef(mods, n, _, argss, _, Block(stats, _)) if n == nme.CONSTRUCTOR =>
            val a = argss.headOption.map(vp => vp.map(v => v.name.decoded)).getOrElse(Nil)
            a
        }
        if (ctor.size != 1) c.abort(c.enclosingPosition, "Only single primary constructor is currently supported. Sorry.")
        val init = ctor.head.map(f => f -> reify(JsIdent(c.literal(f).splice)))
        val defs = body.collect(objectFields)
        val fields = init ::: defs
        val fs = listToExpr(fields.map { case (n, v) => reify((c.literal(n).splice, v.splice)) })
        val args = listToExpr(ctor.head.map(arg => c.literal(arg)))
        reify(JsObjDecl(c.literal(clsName.decoded).splice, args.splice, fs.splice))
    }

    lazy val jsAnonObjDecl: ToExpr[JsAnonObjDecl] = {
      case Block(List(ClassDef(_, clsName, _, Template(_, _, body))), _/* Constructor call */) =>
        val defs = body.collect(objectFields).toMap
        val m = mapToExpr(defs)
        reify(JsAnonObjDecl(m.splice))
    }

    lazy val jsReturn1: ToExpr[JsStmt] = {
      case Return(expr) =>
        reify(JsReturn(jsExpr(expr).splice))
    }
    lazy val jsReturn: ToExpr[JsStmt] = jsReturnStmt orElse jsStmt
    lazy val jsReturnStmt: ToExpr[JsReturn] = jsExpr andThen (jsExpr => reify(JsReturn(jsExpr.splice)))

    lazy val jsBlock: ToExpr[JsBlock] = {
      case Block(stmts, expr) =>
        val stmtTrees = if (expr.equalsStructure(c.literalUnit.tree)) stmts else stmts :+ expr
        val ss = listToExpr(stmtTrees map jsStmt)
        reify(JsBlock(ss.splice))
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
      jsAnonObjDecl
    ) reduceLeft( _ orElse _)
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

    lazy val jsAst: ToExpr[JsAst] = jsExpr orElse jsStmt

    jsAst apply tree
  }
}
