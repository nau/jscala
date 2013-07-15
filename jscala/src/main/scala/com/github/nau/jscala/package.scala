package com.github.nau


sealed trait JsAst

sealed trait JsStmt extends JsAst
sealed trait JsExpr extends JsAst
sealed trait JsLit extends JsExpr

case class JsBool(value: Boolean) extends JsLit
case class JsString(value: String) extends JsLit
case class JsNum(value: Double, isFloat: Boolean) extends JsLit
case object JsUnit extends JsLit with JsStmt

case class JsIdent(ident: String) extends JsExpr
case class JsSelect(qualifier: JsExpr, name: String) extends JsExpr
case class JsUnOp(operator: String, operand: JsExpr) extends JsExpr
case class JsBinOp(operator: String, lhs: JsExpr, rhs: JsExpr) extends JsExpr
case class JsCall(callee: JsExpr, params: List[JsExpr]) extends JsExpr
case class JsAnonFunDecl(params: List[String], body: JsStmt) extends JsExpr
case class JsAnonObjDecl(fields: Map[String, JsExpr]) extends JsExpr

case class JsBlock(stmts: List[JsStmt]) extends JsStmt
case class JsExprStmt(jsExpr: JsExpr) extends JsStmt
case class JsIf(cond: JsExpr, `then`: JsStmt, `else`: Option[JsStmt]) extends JsStmt
case class JsWhile(cond: JsExpr, body: JsStmt) extends JsStmt
case class JsVarDef(ident: String, initializer: JsExpr) extends JsStmt
case class JsFunDecl(ident: String, params: List[String], body: JsStmt) extends JsStmt
case class JsReturn(jsExpr: JsExpr) extends JsStmt

package object jscala {
  import language.experimental.macros

  import scala.reflect.macros.Context


  class ScalaToJsConverter[C <: Context](val c: C) {
    import c.universe._
    type ToExpr[T] = PartialFunction[Tree, Expr[T]]

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

    private lazy val jsStringLit: ToExpr[JsString] = {
      case Literal(Constant(value: Char))  => reify(JsString(c.literal(value).splice.toString))
      case Literal(Constant(value: String))  => reify(JsString(c.literal(value).splice))
    }
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
      println(showRaw(tree))

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

      lazy val stdFuncsMap = Map("replaceAll" -> "replace")
      lazy val stdFuncsSubst = Set("replaceAll")

      lazy val jsStandardFuncs: ToExpr[JsExpr] = {
        case Select(ident, name) if stdFuncsMap.contains(name.encoded) =>
          reify(JsSelect(jsExpr(ident).splice, c.literal(stdFuncsMap(name.encoded)).splice))
        case tree =>
          println("Std Funcs " + showRaw(tree))
          jsExpr(tree)
      }

      lazy val jsCallExpr: ToExpr[JsCall] = {
        case Apply(Select(ident, name), args) if stdFuncsSubst contains name.encoded =>
          name.encoded match {
            case "replaceAll" =>
              val callee = reify(JsSelect(jsExpr(ident).splice, "replace"))
              val params = exprsToExprOfList(args.map(jsExpr) :+ jsStringLit(c.literal("g").tree))
              reify(JsCall(callee.splice, params.splice))
          }
        case Apply(fun, args) =>
          val callee = jsStandardFuncs orElse jsExpr apply fun
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
          println(s"ValDef $name = ${showRaw(rhs)}")
          val identifier = c.literal(name.encoded)
          val initializer = jsExpr(rhs)
          reify(JsVarDef(identifier.splice, initializer.splice))
      }

      lazy val jsFunDecl: ToExpr[JsFunDecl] = {
        case DefDef(_, name, _, vparamss, _, rhs) =>
//          println(showRaw(rhs))
          val ident = c.literal(name.encoded)
          val a = vparamss.headOption.map(vp => vp.map(v => c.literal(v.name.encoded))).getOrElse(Nil)
          val params = exprsToExprOfList(a)
          val body = rhs match {
            case lit@Literal(_) =>
              val body = if (isUnit(lit)) reify(JsUnit) else jsReturnStmt(lit)
              reify(JsBlock(List(body.splice)))
            case Block(stmts, expr) =>
              val lastExpr = if (isUnit(expr)) reify(JsUnit) else jsReturn(expr)
              val ss = exprsToExprOfList(stmts.map(jsStmt) :+ lastExpr)
              reify(JsBlock(ss.splice))
            case _ => reify(JsBlock(List(jsReturn(rhs).splice)))
          }
          reify(JsFunDecl(ident.splice, params.splice, body.splice))
      }

      lazy val jsAnonFunDecl: ToExpr[JsAnonFunDecl] = {
        case Function(vparams, rhs) =>
          val params = exprsToExprOfList(vparams.map(v => c.literal(v.name.encoded)))
          val body = rhs match {
            case lit@Literal(_) =>
              val body = if (isUnit(lit)) reify(JsUnit) else jsReturnStmt(lit)
              reify(JsBlock(List(body.splice)))
            case Block(stmts, expr) =>
              val lastExpr = if (isUnit(expr)) reify(JsUnit) else jsReturn(expr)
              val ss = exprsToExprOfList(stmts.map(jsStmt) :+ lastExpr)
              reify(JsBlock(ss.splice))
            case _ => reify(JsBlock(List(jsReturn(rhs).splice)))
          }
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
        case tree => println("AnonObj " + showRaw(tree)); reify(JsAnonObjDecl(Map.empty))
      }

      lazy val jsReturn: ToExpr[JsStmt] = jsReturnStmt orElse jsStmt
      lazy val jsReturnStmt: ToExpr[JsReturn] = jsExpr andThen (jsExpr => reify(JsReturn(jsExpr.splice)))

      lazy val jsExpr: ToExpr[JsExpr] = Seq(jsLit, jsUnaryOp, jsBinOp, jsCallExpr, jsAnonFunDecl, jsSelect, jsIdent, jsAnonObjDecl) reduceLeft( _ orElse _)
      lazy val jsExprStmt: ToExpr[JsExprStmt] = jsExpr andThen (jsExpr => reify(JsExprStmt(jsExpr.splice)))

      lazy val jsStmt: ToExpr[JsStmt] = Seq(
        jsUnitLit,
        jsBlock,
        jsVarDefStmt,
        jsExprStmt,
        jsIfStmt,
        jsWhileStmt,
        jsReturnStmt,
        jsFunDecl
      ) reduceLeft (_ orElse _)

      lazy val jsBlock: ToExpr[JsBlock] = {
        case Block(stmts, expr) =>
          val stmtTrees = stmts :+ expr
          val ss = exprsToExprOfList(stmtTrees map jsStmt)
          reify(JsBlock(ss.splice))
      }

      jsBlock orElse jsExpr apply tree
    }
  }

  def javascript(expr: Any): JsAst = macro javascriptImpl
  def javascriptImpl(c: Context)(expr: c.Expr[Any]) = {
    val parser = new ScalaToJsConverter[c.type](c)
    parser.convert(expr.tree)
  }
}
