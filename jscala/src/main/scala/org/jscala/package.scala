package org

import javax.script.ScriptEngineManager
import com.yahoo.platform.yui.compressor.JavaScriptCompressor
import java.io.{StringWriter, StringReader}
import org.mozilla.javascript.ErrorReporter
import scala.reflect.internal.Flags

package object jscala {
  import language.experimental.macros
  import scala.reflect.macros.Context

  implicit class JsAstOps(ast: JsAst) {
    def asString = JavascriptPrinter.print(ast, 0)
    def eval() = {
      val factory = new ScriptEngineManager()
      val engine = factory.getEngineByName("JavaScript")
      engine.eval(asString)
    }
    def compress = {
      val compressor = new JavaScriptCompressor(new StringReader(asString), new ErrorReporter {
        def warning(p1: String, p2: String, p3: Int, p4: String, p5: Int) {
          println(s"Warn $p1 $p2, ${p3.toString} $p4 ${p5.toString}")
        }

        def error(p1: String, p2: String, p3: Int, p4: String, p5: Int) {
          println(s"Error $p1 $p2, ${p3.toString} $p4 ${p5.toString}")
        }

        def runtimeError(p1: String, p2: String, p3: Int, p4: String, p5: Int) = {
          println(s"Runtime $p1 $p2, ${p3.toString} $p4 ${p5.toString}")
          ???
        }
      })
      val buf = new StringWriter
      compressor.compress(buf, 1, true, false, false, false)
      buf.toString
    }
  }

  trait JsSerializer[A] {
    def apply(a: A): JsExpr
  }
  implicit object boolJsSerializer extends JsSerializer[Boolean] { def apply(a: Boolean) = JsBool(a) }
  implicit object byteJsSerializer extends JsSerializer[Byte] { def apply(a: Byte) = JsNum(a, false) }
  implicit object shortJsSerializer extends JsSerializer[Short] { def apply(a: Short) = JsNum(a, false) }
  implicit object intJsSerializer extends JsSerializer[Int] { def apply(a: Int) = JsNum(a, false) }
  implicit object longJsSerializer extends JsSerializer[Long] { def apply(a: Long) = JsNum(a, false) }
  implicit object floatJsSerializer extends JsSerializer[Float] { def apply(a: Float) = JsNum(a, true) }
  implicit object doubleJsSerializer extends JsSerializer[Double] { def apply(a: Double) = JsNum(a, true) }
  implicit object stringJsSerializer extends JsSerializer[String] { def apply(a: String) = JsString(a) }
  implicit object arrJsSerializer extends JsSerializer[collection.Seq[JsExpr]] { def apply(a: collection.Seq[JsExpr]) = JsArray(a.toList) }
  implicit object mapJsSerializer extends JsSerializer[collection.Map[String,JsExpr]] { def apply(a: collection.Map[String,JsExpr]) = JsAnonObjDecl(a.toMap) }
  implicit def funcJsSerializer[A](implicit ev: JsSerializer[A]): JsSerializer[() => A] = new JsSerializer[() => A] { def apply(a: () => A) = ev.apply(a()) }
  implicit class ToJsExpr[A](a: A)(implicit ev: JsSerializer[A]) {
    def toJs: JsExpr = ev.apply(a)
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

    private val jsLit: ToExpr[JsLit] = {
      jsStringLit orElse jsNumLit orElse jsBoolLit orElse jsUnitLit
    }

    private def select(p: String, init: String => Tree = s => Ident(newTermName(s))): Tree = {
      p.split("\\.").foldLeft(EmptyTree) {
        case (EmptyTree, el) => init(el)
        case (t, el) => Select(t, newTermName(el))
      }
    }

    implicit class TreeHelper(tree: Tree) {
      def is(p: String) = tree.equalsStructure(select(p)) || tree.equalsStructure(select(p, s => This(newTypeName(s))))
    }

    object Name {
      def unapply(name: Name) = Some(name.decoded)
    }

    def convert(tree: Tree): c.Expr[JsAst] = {
//      println(showRaw(tree))

      lazy val jsThis: ToExpr[JsIdent] = {
        case This(name) => reify(JsIdent("this"))
      }

      lazy val jsIdent: ToExpr[JsIdent] = {
        case Ident(name) => reify(JsIdent(c.literal(name.decoded).splice))
      }

      lazy val jsSelect: ToExpr[JsSelect] = {
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
          jsString(lhs) -> jsExpr(rhs)
        }
        val params = mapToExpr(map.toMap)
        reify(JsAnonObjDecl(params.splice))
      }

      lazy val jsMapExpr: ToExpr[JsExpr] = {
        case Apply(Select(Ident(ident), Name("apply")), List(index)) =>
          reify(JsAccess(JsIdent(c.literal(ident.decoded).splice), jsExpr(index).splice))
        case Apply(Select(Ident(ident), Name("update")), List(key, value)) =>
          reify(JsBinOp("=", JsAccess(JsIdent(c.literal(ident.decoded).splice), jsExpr(key).splice), jsExpr(value).splice))
        case Apply(TypeApply(path, _), args) if path.is("scala.Predef.Map.apply") =>
          genMap(args)
        case Apply(TypeApply(path, _), args) if path.is("scala.collection.mutable.Map.apply") =>
          genMap(args)
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
        case Apply(TypeApply(path, _), args) if path.is("JArray.apply") =>
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
      }

      lazy val jsGlobalFuncsExpr: ToExpr[JsExpr] = {
        case TypeApply(Select(expr, Name("asInstanceOf")), _) => jsExpr(expr)
        case Apply(path, List(Literal(Constant(js: String)))) if path.is("jscala.package.include") =>
          reify(JsRaw(c.literal(js).splice))
        case app@Apply(Apply(TypeApply(path, _), List(ident)), List(jss)) if path.is("jscala.package.inject") =>
          val call = c.Expr[JsExpr](Apply(jss, List(ident)))
          reify(JsLazy(() => call.splice))
        case Apply(Select(path, fn), args) if path.is("jscala.package") =>
          val params = listToExpr(args map jsExpr)
          reify(JsCall(JsIdent(c.literal(fn.decoded).splice), params.splice))
      }

      lazy val jsJStringExpr: ToExpr[JsExpr] = {
        case Apply(Select(New(Ident(Name("JString"))), _), List(Literal(Constant(str: String)))) =>
          reify(JsString(c.literal(str).splice))
      }

      lazy val jsNewExpr: ToExpr[JsExpr] = {
        case Apply(Select(New(Ident(ident)), _), args) =>
          val params = listToExpr(args map jsExpr)
          reify(JsNew(JsCall(JsIdent(c.literal(ident.decoded).splice), params.splice)))
      }

      lazy val jsCallExpr: ToExpr[JsExpr] = {
        case Apply(Select(lhs, name), List(rhs)) if name.decoded.endsWith("_=") =>
          reify(JsBinOp("=", JsSelect(jsExpr(lhs).splice, c.literal(name.decoded.dropRight(2)).splice), jsExpr(rhs).splice))
        case Apply(fun, args) =>
          val callee = jsExpr apply fun
          val filteredDefaults = args collect {
            case arg@Select(_, n) if n.decoded.contains("$default$") => None
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

      lazy val jsVarDefStmt: ToExpr[JsVarDef] = {
        case ValDef(_, name, _, rhs) =>
          val identifier = c.literal(name.decoded)
          val initializer = jsExpr(rhs)
          reify(JsVarDef(identifier.splice, initializer.splice))
      }

      lazy val jsFunBody: ToExpr[JsBlock] = {
        case lit@Literal(_) =>
          val body = if (isUnit(lit)) Nil else List(jsReturnStmt(lit))
          reify(JsBlock(listToExpr(body).splice))
        case Block(stmts, expr) =>
          val lastExpr = if (isUnit(expr)) Nil else List(jsReturn(expr))
          val ss = listToExpr(stmts.map(jsStmt) ::: lastExpr)
          reify(JsBlock(ss.splice))
        case rhs => reify(JsBlock(List(jsReturn(rhs).splice)))
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


      lazy val jsSwitch: ToExpr[JsSwitch] = {
        case Match(expr, cases) =>
          val cs = cases collect {
            case CaseDef(const@Literal(Constant(_)), EmptyTree, body) => List(reify(JsCase(jsLit(const).splice, jsStmt(body).splice)))
            case CaseDef(Alternative(xs), EmptyTree, body) =>
              val stmt = jsStmt(body)
              for (const <- xs) yield reify(JsCase(jsLit(const).splice, stmt.splice))
          }
          val df = (cases collect {
            case CaseDef(Ident(nme.WILDCARD), EmptyTree, body) => reify(Some(JsDefault(jsStmt(body).splice)))
          }).headOption.getOrElse(reify(None))
        val css = listToExpr(cs.flatten)
        reify(JsSwitch(jsExpr(expr).splice, css.splice, df.splice))
      }

      lazy val jsClassDecl: ToExpr[JsObjDecl] = {
        case ClassDef(_, clsName, _, Template(_, _, body)) =>
          val defs = body.collect {
            case f@DefDef(mods, n, _, argss, _, body) if n != nme.CONSTRUCTOR && !mods.hasFlag(Flags.ACCESSOR.toLong.asInstanceOf[FlagSet]) =>
              n.decoded -> jsExpr(Function(argss.flatten, body))
            case ValDef(_, n, _, rhs) =>
              // For some reason there is an extra space in a field name. Trim it.
              n.decoded.trim -> jsExpr(rhs)
          }.toMap
          val m = mapToExpr(defs)
          reify(JsObjDecl(m.splice))
      }

      lazy val jsAnonObjDecl: ToExpr[JsAnonObjDecl] = {
        case Block(List(ClassDef(_, clsName, _, Template(_, _, body))), _/* Constructor call */) =>
          val defs = body.collect {
            case f@DefDef(mods, n, _, argss, _, body) if n != nme.CONSTRUCTOR && !mods.hasFlag(Flags.ACCESSOR.toLong.asInstanceOf[FlagSet]) =>
              n.decoded -> jsExpr(Function(argss.flatten, body))
            case ValDef(_, n, _, rhs) =>
              // For some reason there is an extra space in a field name. Trim it.
              n.decoded.trim -> jsExpr(rhs)
          }.toMap
          val m = mapToExpr(defs)
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
        jsThis,
        jsAnonObjDecl
      ) reduceLeft( _ orElse _)
      lazy val jsExprStmt: ToExpr[JsExprStmt] = jsExpr andThen (jsExpr => reify(JsExprStmt(jsExpr.splice)))

      lazy val jsStmt: ToExpr[JsStmt] = Seq(
        jsUnitLit,
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

      lazy val jsBlock: ToExpr[JsBlock] = {
        case Block(stmts, expr) =>
          val stmtTrees = stmts :+ expr
          val ss = listToExpr(stmtTrees map jsStmt)
          reify(JsBlock(ss.splice))
      }
      jsExpr orElse jsStmt apply tree
    }
  }

  def inject[A](a: A)(implicit jss: JsSerializer[A]) = a

  def javascript[A](expr: A): JsAst = macro javascriptImpl
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
  def typeof(x: Any) = ""
  def include(js: String) = ""
}
