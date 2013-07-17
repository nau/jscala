package org

import javax.script.ScriptEngineManager

package object jscala {
  import language.experimental.macros

  import scala.reflect.macros.Context
  implicit class JsAstOps(ast: JsAst) {
    def print = JavascriptPrinter.print(ast, 0)
    def eval() = {
      val factory = new ScriptEngineManager()
      // create a JavaScript engine
      val engine = factory.getEngineByName("JavaScript")
      // evaluate JavaScript code from String
      engine.eval(print)
    }
  }

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

    private def isUnit(tree: Tree) = tree.equalsStructure(c.literalUnit.tree)
    private def exprsToExprOfList[T](exprs: List[Expr[T]]): Expr[List[T]] = c.Expr[List[T]](treeBuild.mkMethodCall(reify(List).tree, exprs.map(_.tree)))
    private def mapToExprOfMap[V](m: Map[String, Expr[V]]): Expr[Map[String, V]] = {
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

    private val jsLit: ToExpr[JsLit] = {
      jsStringLit orElse jsNumLit orElse jsBoolLit orElse jsUnitLit
    }

    def convert(tree: Tree): c.Expr[JsAst] = {

      lazy val jsIdent: ToExpr[JsIdent] = {
        case Ident(name) => reify(JsIdent(c.literal(name.encoded).splice))
      }

      lazy val jsSelect: ToExpr[JsSelect] = {
        case Select(q, name) => reify(JsSelect(jsExpr(q).splice, c.literal(name.encoded).splice))
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
        case Apply(TypeApply(Select(Apply(TypeApply(Select(Select(This(name), predef), arrowAssoc), _), List(lhs)), arrow), _), List(rhs))
          if name == newTypeName("scala") && predef == newTermName("Predef") && arrowAssoc == newTermName("any2ArrowAssoc")
            && (arrow.decoded == "->" || arrow.decoded == "→") =>
        lhs -> rhs
        case Apply(TypeApply(Select(Select(Ident(name), tuple2), apply), _), List(lhs, rhs))
          if name == newTermName("scala") && tuple2 == newTermName("Tuple2") && apply == newTermName("apply") =>
          lhs -> rhs
      }

      lazy val jsMapExpr: ToExpr[JsExpr] = {
        case Apply(Select(Ident(ident), apply), List(index)) if apply == newTermName("apply") =>
          reify(JsAccess(JsIdent(c.literal(ident.decoded).splice), jsExpr(index).splice))
        case Apply(TypeApply(Select(Select(Select(This(name), predef), map), apply), _), args)
          if name == newTypeName("scala") && predef == newTermName("Predef") && map == newTermName("Map") && apply == newTermName("apply") =>
          val map = for (arg <- args) yield {
            val (lhs, rhs) = jsTupleExpr(arg)
            jsString(lhs) -> jsExpr(rhs)
          }
          val params = mapToExprOfMap(map.toMap)
          reify(JsAnonObjDecl(params.splice))
      }

      lazy val jsForStmt: ToExpr[JsStmt] = {
        case Apply(TypeApply(Select(Apply(TypeApply(Select(Select(This(name), predef), arrayOps), _), List(Ident(coll))), fn), _), List(Function(List(ValDef(_, ident, _, _)), body)))
          if name == newTypeName("scala") && predef == newTermName("Predef") && arrayOps == newTermName("refArrayOps") && fn == newTermName("foreach") =>
          val forBody = jsStmt(body)
          reify(JsFor(JsIdent(c.literal(coll.decoded).splice), JsIdent(c.literal(ident.decoded).splice), forBody.splice))
      }

      lazy val jsArrayExpr: ToExpr[JsExpr] = {
        case Apply(TypeApply(Select(Ident(jarray), apply), _), args) if jarray == newTermName("JArray") && apply == newTermName("apply") =>
          val params = exprsToExprOfList(args map jsExpr)
          reify(JsArray(params.splice))
        case TypeApply(Select(Select(Ident(name), array), apply), args) if name == newTermName("scala") && array == newTermName("Array") && apply == newTermName("apply") =>
          val params = exprsToExprOfList(args map jsExpr)
          reify(JsArray(params.splice))
        case Apply(Apply(TypeApply(Select(Select(Ident(name), array), apply), _), args), _) if name == newTermName("scala") && array == newTermName("Array") && apply == newTermName("apply") =>
          val params = exprsToExprOfList(args map jsExpr)
          reify(JsArray(params.splice))
        case TypeApply(Select(Select(Ident(name), array), apply), args) if name == newTermName("scala") && array == newTermName("Array") && apply == newTermName("apply") =>
          val params = exprsToExprOfList(args map jsExpr)
          reify(JsArray(params.splice))
        case Apply(Select(Select(Ident(name), array), apply), args) if name == newTermName("scala") && array == newTermName("Array") && apply == newTermName("apply") =>
          val params = exprsToExprOfList(args map jsExpr)
          reify(JsArray(params.splice))
      }

      lazy val jsGlobalFuncsExpr: ToExpr[JsExpr] = {
        case Apply(Select(Select(This(jscala), pkg), fn), args) if jscala == newTypeName("jscala") && pkg == newTermName("package") =>
          val params = exprsToExprOfList(args map jsExpr)
          reify(JsCall(JsIdent(c.literal(fn.decoded).splice), params.splice))
      }

      lazy val jsJStringExpr: ToExpr[JsExpr] = {
        case Apply(Select(New(Ident(jstring)), _), List(Literal(Constant(str: String)))) if jstring == newTypeName("JString") =>
          reify(JsString(c.literal(str).splice))
      }

      lazy val jsNewExpr: ToExpr[JsExpr] = {
        case Apply(Select(New(Ident(ident)), _), args) =>
          val params = exprsToExprOfList(args map jsExpr)
          reify(JsNew(JsCall(JsIdent(c.literal(ident.decoded).splice), params.splice)))
      }

      lazy val jsCallExpr: ToExpr[JsExpr] = {
        case Apply(fun, args) =>
          val callee = jsExpr apply fun
          val params = exprsToExprOfList(args.map(jsExpr))
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

      lazy val jsVarDefStmt: ToExpr[JsVarDef] = {
        case ValDef(_, name, _, rhs) =>
          val identifier = c.literal(name.encoded)
          val initializer = jsExpr(rhs)
          reify(JsVarDef(identifier.splice, initializer.splice))
      }

      lazy val jsFunBody: ToExpr[JsBlock] = {
        case lit@Literal(_) =>
          val body = if (isUnit(lit)) Nil else List(jsReturnStmt(lit))
          reify(JsBlock(exprsToExprOfList(body).splice))
        case Block(stmts, expr) =>
          val lastExpr = if (isUnit(expr)) Nil else List(jsReturn(expr))
          val ss = exprsToExprOfList(stmts.map(jsStmt) ::: lastExpr)
          reify(JsBlock(ss.splice))
        case rhs => reify(JsBlock(List(jsReturn(rhs).splice)))
      }

      lazy val jsFunDecl: ToExpr[JsFunDecl] = {
        case DefDef(_, name, _, vparamss, _, rhs) =>
          val ident = c.literal(name.encoded)
          val a = vparamss.headOption.map(vp => vp.map(v => c.literal(v.name.encoded))).getOrElse(Nil)
          val params = exprsToExprOfList(a)
          val body = jsFunBody(rhs)
          reify(JsFunDecl(ident.splice, params.splice, body.splice))
      }

      lazy val jsAnonFunDecl: ToExpr[JsAnonFunDecl] = {
        case Function(vparams, rhs) =>
          val params = exprsToExprOfList(vparams.map(v => c.literal(v.name.encoded)))
          val body = jsFunBody(rhs)
          reify(JsAnonFunDecl(params.splice, body.splice))
      }

      lazy val jsAnonObjDecl: ToExpr[JsAnonObjDecl] = {
        case Block(List(ClassDef(_, _, _, Template(_, _, body))), _) =>
          val defs = body.collect {
//            case DefDef(_, n, _, _, _, rhs) if n != nme.CONSTRUCTOR => n.encoded -> jsExpr(rhs)
            case ValDef(_, n, _, rhs) => n.decoded -> jsExpr(rhs)
          }.toMap
          val m = mapToExprOfMap(defs)
          reify(JsAnonObjDecl(m.splice))
      }

      lazy val jsReturn1: ToExpr[JsStmt] = {
        case Return(expr) =>
          reify(JsReturn(jsExpr(expr).splice))
      }
      lazy val jsReturn: ToExpr[JsStmt] = jsReturnStmt orElse jsStmt
      lazy val jsReturnStmt: ToExpr[JsReturn] = jsExpr andThen (jsExpr => reify(JsReturn(jsExpr.splice)))

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
        jsAnonObjDecl
      ) reduceLeft( _ orElse _)
      lazy val jsExprStmt: ToExpr[JsExprStmt] = jsExpr andThen (jsExpr => reify(JsExprStmt(jsExpr.splice)))

      lazy val jsStmt: ToExpr[JsStmt] = Seq(
        jsUnitLit,
        jsBlock,
        jsVarDefStmt,
        jsIfStmt,
        jsWhileStmt,
        jsForStmt,
        jsFunDecl,
        jsReturn1,
        jsExprStmt
      ) reduceLeft (_ orElse _)

      lazy val jsBlock: ToExpr[JsBlock] = {
        case Block(stmts, expr) =>
          val stmtTrees = stmts :+ expr
          val ss = exprsToExprOfList(stmtTrees map jsStmt)
          reify(JsBlock(ss.splice))
      }

      jsExpr orElse jsStmt apply tree
    }
  }

  def javascript(expr: Any): JsAst = macro javascriptImpl
  def javascriptImpl(c: Context)(expr: c.Expr[Any]) = {
    val parser = new ScalaToJsConverter[c.type](c)
    parser.convert(expr.tree)
  }


  val Infinity = Double.PositiveInfinity
  val NaN = Double.NaN
  val undefined: AnyRef = null
  // Javascript Global Functions
  def decodeURI(uri: String): JString = null
  def decodeURIComponent(uri: String): JString = null
  def encodeURI(uri: String): JString = null
  def encodeURIComponent(uri: String): JString = null
  def escape(uri: String): JString = null
  def unescape(uri: String): JString = null
  def eval(uri: String): AnyRef = null
  def isFinite(uri: AnyRef) = false
  def isNaN(uri: AnyRef) = false
  def parseFloat(str: String) = 1.0
  def parseInt()(str: String) = 1
}
