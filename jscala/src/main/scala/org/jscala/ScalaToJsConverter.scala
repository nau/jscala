package org.jscala

import scala.language.experimental.macros
import scala.language.implicitConversions
import scala.reflect.internal.Flags
import scala.reflect.macros.blackbox

class ScalaToJsConverter[C <: blackbox.Context](val c: C, debug: Boolean) extends SyntaxConverter[C] with CollectionConverter[C] {
  import c.universe._

  private val traits = collection.mutable.HashMap[String, List[Tree]]()
  private val ints = Set("Byte", "Short", "Int", "Long", "Float", "Double")

  def convert(tree: Tree): Tree = {
    if (debug) println(tree)
    if (debug) println(tree.raw)

    val expr = try jsAst apply tree catch {
      case ex: DieException =>
        val stack = if (debug) Seq("Raw tree:", ex.t.raw) ++ Thread.currentThread.getStackTrace mkString "\n" else ""
        c.abort(ex.t.pos, s"${ex.msg}: ${ex.t}. $stack")
    }

    val resultTree = if (!tree.tpe.=:=(typeOf[Nothing]) && functionTypes.exists(tree.tpe.<:<)) {
      q"$expr.asInstanceOf[org.jscala.JsAst with ${tree.tpe}]"
    } else expr

    resultTree
  }

  private lazy val jsCaseClassApply: ToExpr[JsAnonObjDecl] = {
    case tree@Apply(Select(path, TermName("apply")), args) if tree.isCaseClass && !tree.tpe.typeSymbol.fullName.startsWith("org.jscala.") =>
      val params = listToExpr(tree.caseMembers.zip(args).map { case (m, a) => q"(${m.name.decodedName.toString}, ${jsExprOrDie(a)})" })
      q"org.jscala.JsAnonObjDecl($params)"
  }

  private def typeConverter(tpe: Tree): String = {
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

  private lazy val jsGlobalFuncsExpr: ToExpr[JsExpr] = {
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
    case q"js.this.Any.${TermName(func)}[..$tpts](...$exprss)" if func.startsWith("from") || func.startsWith("to") =>
      jsExprOrDie(exprss.head.head)
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

  private def eligibleDef(f: DefDef) = {
    f.name != termNames.CONSTRUCTOR && f.name.decodedName.toString != "$init$" && !f.mods.hasFlag(Flags.ACCESSOR.toLong.asInstanceOf[FlagSet] | Flag.DEFERRED)
  }

  private lazy val objectFields: PFT[(String, Tree)] = {
    case f@DefDef(mods, n, _, argss, _, body) if eligibleDef(f) =>
      n.decodedName.toString -> jsExprOrDie(Function(argss.flatten, body))
    case ValDef(mods, n, _, rhs) if !rhs.equalsStructure(EmptyTree)
      && !mods.hasFlag(Flags.PARAMACCESSOR.toLong.asInstanceOf[FlagSet] | Flag.ABSTRACT) => n.decodedName.toString.trim -> jsExprOrDie(rhs)
  }

  private lazy val jsClassDecl: ToExpr[JsStmt] = {
    case cd@ClassDef(mods, clsName, _, Template(_, _, body)) if mods.hasFlag(Flag.TRAIT) =>
      // Remember trait AST to embed its definitions in concrete class for simplicity
      traits(cd.symbol.fullName) = body
      q"org.jscala.JsUnit"
    case cd@ClassDef(mods, clsName, _, Template(_, _, body)) if mods.hasFlag(Flag.CASE) =>
      q"org.jscala.JsUnit"
    case ModuleDef(_, name, _) =>
      q"org.jscala.JsUnit"
    case cd@ClassDef(_, clsName, _, t@Template(base, _, body)) =>
      val ctor = body.collect {
        case f@DefDef(mods, n, _, argss, _, Block(stats, _)) if n == termNames.CONSTRUCTOR =>
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

  private lazy val jsAnonObjDecl: ToExpr[JsAnonObjDecl] = {
    case Block(List(ClassDef(_, clsName, _, Template(_, _, body))), _/* Constructor call */) =>
      val defs = body.collect(objectFields)
      val params = listToExpr(defs.map { case (n, v) => q"($n, $v)" })
      q"org.jscala.JsAnonObjDecl($params)"
  }

  /** Case class creation, using named arguments in wrong order
    * {
    * <artifact> val x$2: Int = 2;
    * <artifact> val x$3: String = "nau";
    * User.apply(x$3, x$2)
    * }
    */
  private lazy val jsCaseClassNamedArgs: ToExpr[JsExpr] = {
    case Block(valdefs, ap@Apply(expr, params)) if jsCaseClassApply.isDefinedAt(ap) =>
      val args = valdefs.map { case ValDef(_, TermName(n), _, init) => n -> init }.toMap
      val orderedParams = params.map { case Ident(TermName(i)) => args(i) }
      val tree1 = c.typecheck(Apply(expr, orderedParams))
      jsCaseClassApply(tree1)
  }

  protected lazy val jsExpr: ToExpr[JsExpr] = Seq(
    jsLit,
    jsUnaryOp,
    jsBinOp,
    jsGlobalFuncsExpr,
    jsStringHelpersExpr,
    jsStringInterpolation,
    jsJStringExpr,
    jsArrayExpr,
    jsNewExpr,
    jsCaseClassApply,
    jsMapExpr,
    jsCallExpr,
    jsAnonFunDecl,
    jsSelect,
    jsIdent,
    jsThis,
    jsAnonObjDecl,
    jsThrowExpr,
    jsTernaryExpr,
    jsCaseClassNamedArgs
  ) reduceLeft( _ orElse _)

  protected lazy val jsStmt: ToExpr[JsStmt] = Seq(
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

  private lazy val jsAst: ToExpr[JsAst] = jsStmtOrDie
}
