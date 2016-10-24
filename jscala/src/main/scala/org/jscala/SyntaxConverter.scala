package org.jscala

import scala.reflect.macros.blackbox

/**
 * Author: Alexander Nemish
 */
trait SyntaxConverter[C <: blackbox.Context] extends BasisConverter[C] {
  import c.universe._

  protected lazy val jsReturn1: ToExpr[JsStmt] = {
    case Return(expr) => q"org.jscala.JsReturn(${jsExprOrDie(expr)})"
  }

  protected lazy val jsReturn: ToExpr[JsStmt] = jsReturnStmt orElse jsStmtOrDie

  protected lazy val jsReturnStmt: ToExpr[JsReturn] = jsExpr andThen (jsExpr => q"org.jscala.JsReturn($jsExpr)")

  protected lazy val jsIfStmt: ToExpr[JsIf] = {
    case q"if ($cond) $thenp else $elsep" =>
      val condJsExpr = jsExprOrDie(cond)
      val thenJsExpr = jsStmtOrDie(thenp)
      val elseJsStmt = if (isUnit(elsep)) q"None" else q"Some(${jsStmtOrDie(elsep)})"
      q"org.jscala.JsIf($condJsExpr, $thenJsExpr, $elseJsStmt)"
  }

  protected lazy val jsTernaryExpr: ToExpr[JsTernary] = {
    case 	q"if ($cond) $thenp else $elsep" if !thenp.tpe.=:=(typeOf[Unit]) && !isUnit(elsep) && jsExpr.isDefinedAt(thenp) && jsExpr.isDefinedAt(elsep) =>
      val condJsExpr = jsExprOrDie(cond)
      val thenJsExpr = jsExprOrDie(thenp)
      val elseExpr = jsExprOrDie(elsep)
      q"org.jscala.JsTernary($condJsExpr, $thenJsExpr, $elseExpr)"
  }

  protected lazy val jsWhileStmt: ToExpr[JsWhile] = {
    case q"while ($cond) $body" =>
      val condJsExpr = jsExprOrDie(cond)
      val bodyJsStmt = jsStmtOrDie(body)
      q"org.jscala.JsWhile($condJsExpr, $bodyJsStmt)"
  }

  private def addAssign(tree: Tree, name: Name) = tree match {
    case Block(stats, expr) => Block(stats :+ Assign(Ident(name), expr), q"()")
    case expr => Block(Assign(Ident(name), expr) :: Nil, q"()")
  }

  protected lazy val jsIfExpr: PartialFunction[(Name, Tree), Tree] = {
    case (name, If(cond, thenp, elsep)) =>
      val condJsExpr = jsExprOrDie(cond)
      val thenJsExpr = jsStmtOrDie(addAssign(thenp, name))
      val elseJsExpr = jsStmtOrDie(addAssign(elsep, name))
      q"org.jscala.JsIf($condJsExpr, $thenJsExpr, Some($elseJsExpr))"
  }

  protected lazy val jsMatchExpr: PartialFunction[(Name, Tree), Tree] = {
    case (name, Match(expr, cases)) => jsSwitchGen(expr, cases, body => addAssign(body, name))
  }

  protected lazy val jsVarDefStmt: ToExpr[JsStmt] = {
    case ValDef(_, name, _, rhs) =>
      val identifier = name.decodedName.toString
      if (jsTernaryExpr.isDefinedAt(rhs)) {
        q"org.jscala.JsVarDef($identifier, ${jsTernaryExpr(rhs)})"
      } else {
        val funcs = Seq(jsIfExpr, jsMatchExpr).reduceLeft(_ orElse _) andThen { expr =>
          q"org.jscala.JsStmts(List(org.jscala.JsVarDef($identifier, org.jscala.JsUnit), $expr))"
        }
        val x = name -> rhs
        funcs.applyOrElse(x, (t: (TermName, Tree)) => {
          q"org.jscala.JsVarDef($identifier, ${jsExprOrDie(rhs)})"
        })
      }
  }

  protected lazy val jsFunBody: ToExpr[JsBlock] = {
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

  protected lazy val jsFunDecl: ToExpr[JsFunDecl] = {
    case DefDef(_, name, _, vparamss, _, rhs) =>
      val ident = name.decodedName.toString
      val a = vparamss.headOption.map(vp => vp.map(v => q"${v.name.decodedName.toString}")).getOrElse(Nil)
      val params = listToExpr(a)
      val body = jsFunBody(rhs)
      q"org.jscala.JsFunDecl($ident, $params, $body)"
  }

  protected lazy val jsAnonFunDecl: ToExpr[JsAnonFunDecl] = {
    case Block(Nil, Function(vparams, rhs)) =>
      val params = listToExpr(vparams.map(v => q"${v.name.decodedName.toString}"))
      val body = jsFunBody(rhs)
      q"org.jscala.JsAnonFunDecl($params, $body)"
    case Function(vparams, rhs) =>
      val params = listToExpr(vparams.map(v => q"${v.name.decodedName.toString}"))
      val body = jsFunBody(rhs)
      q"org.jscala.JsAnonFunDecl($params, $body)"
  }

  protected lazy val jsTry: ToExpr[JsTry] = {
    case q"try $body catch { case ..$catchBlock } finally $finBody" =>
      //      case Try(body, catchBlock, finBody) =>
      val ctch = catchBlock match {
        case Nil => q"None"
        case List(CaseDef(Bind(pat, _), EmptyTree, catchBody)) =>
          q"Some(org.jscala.JsCatch(org.jscala.JsIdent(${pat.decodedName.toString}), ${jsStmtOrDie(catchBody)}))"
      }
      val fin = if (finBody.equalsStructure(EmptyTree)) q"None"
      else q"Some(${jsStmtOrDie(finBody)})"
      q"org.jscala.JsTry(${jsStmtOrDie(body)}, $ctch, $fin)"
  }

  protected lazy val jsThrowExpr: ToExpr[JsThrow] = {
    case Throw(expr) => q"org.jscala.JsThrow(${jsExprOrDie(expr)})"
  }

  private def jsSwitchGen(expr: Tree, cases: List[CaseDef], f: Tree => Tree) = {
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
      case CaseDef(Ident(termNames.WILDCARD), EmptyTree, body) => q"Some(org.jscala.JsDefault(${jsStmtOrDie(f(body))}))"
    }).headOption.getOrElse(q"None")
    val css = listToExpr(cs)
    q"org.jscala.JsSwitch(${jsExprOrDie(expr)}, $css, $df)"
  }

  protected lazy val jsSwitch: ToExpr[JsSwitch] = {
    case Match(expr, cases) => jsSwitchGen(expr, cases, t => t)
  }
}
