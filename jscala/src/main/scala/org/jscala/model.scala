package org.jscala


sealed trait JsAst {
  def block: JsBlock
  def join(a: JsAst): JsAst
}

sealed trait JsStmt extends JsAst {
  def block = JsBlock(List(this))
  def join(a: JsAst) = (this, a) match {
    case (JsBlock(lhs), JsBlock(rhs)) => JsBlock(lhs ::: rhs)
    case (JsStmts(lhs), JsBlock(rhs)) => JsBlock(lhs ::: rhs)
    case (JsBlock(lhs), JsStmts(rhs)) => JsBlock(lhs ::: rhs)
    case (JsStmts(lhs), JsStmts(rhs)) => JsStmts(lhs ::: rhs)
    case (JsBlock(lhs), s: JsStmt) => JsBlock(lhs :+ s)
    case (JsBlock(lhs), s: JsExpr) => JsBlock(lhs :+ s.stmt)
    case (s: JsStmt, JsBlock(rhs)) => JsBlock(s :: rhs)
    case (JsStmts(lhs), s: JsStmt) => JsStmts(lhs :+ s)
    case (JsStmts(lhs), s: JsExpr) => JsStmts(lhs :+ s.stmt)
    case (s: JsStmt, JsStmts(rhs)) => JsStmts(s :: rhs)
    case (lhs: JsStmt, rhs: JsStmt) => JsBlock(List(lhs, rhs))
    case (lhs: JsStmt, rhs: JsExpr) => JsBlock(List(lhs, rhs.stmt))
  }
}
sealed trait JsExpr extends JsAst {
  def stmt = JsExprStmt(this)
  def block = JsBlock(List(this.stmt))
  def join(a: JsAst) = stmt.join(a)
}
sealed trait JsLit extends JsExpr

case class JsBool(value: Boolean) extends JsLit
case class JsString(value: String) extends JsLit
case class JsNum(value: Double, isFloat: Boolean) extends JsLit
case class JsArray(values: List[JsExpr]) extends JsLit
case object JsUnit extends JsLit
case object JsNull extends JsLit

case class JsLazy(ast: () => JsAst) extends JsExpr
case class JsIdent(ident: String) extends JsExpr
case class JsRaw(js: String) extends JsExpr
case class JsAccess(qualifier: JsExpr, key: JsExpr) extends JsExpr
case class JsSelect(qualifier: JsExpr, name: String) extends JsExpr
case class JsUnOp(operator: String, operand: JsExpr) extends JsExpr
case class JsBinOp(operator: String, lhs: JsExpr, rhs: JsExpr) extends JsExpr
case class JsCall(callee: JsExpr, params: List[JsExpr]) extends JsExpr
case class JsNew(ctor: JsCall) extends JsExpr
case class JsThrow(expr: JsExpr) extends JsExpr
case class JsAnonFunDecl(params: List[String], body: JsStmt) extends JsExpr
case class JsAnonObjDecl(fields: List[(String, JsExpr)]) extends JsExpr
case class JsTernary(cond: JsExpr, `then`: JsExpr, `else`: JsExpr) extends JsExpr

case class JsBlock(stmts: List[JsStmt]) extends JsStmt
case class JsTry(body: JsStmt, cat: Option[JsCatch], fin: Option[JsStmt]) extends JsStmt
case class JsCatch(ident: JsIdent, body: JsStmt) extends JsStmt
case class JsExprStmt(jsExpr: JsExpr) extends JsStmt
case class JsIf(cond: JsExpr, `then`: JsStmt, `else`: Option[JsStmt]) extends JsStmt
case class JsWhile(cond: JsExpr, body: JsStmt) extends JsStmt
case class JsFor(init: List[JsStmt], check: JsExpr, update: List[JsStmt], body: JsStmt) extends JsStmt
case class JsForIn(ident: JsIdent, coll: JsExpr, body: JsStmt) extends JsStmt
sealed trait JsSwitchable extends JsStmt
case class JsCase(const: List[JsExpr], body: JsStmt) extends JsSwitchable
case class JsDefault(body: JsStmt) extends JsSwitchable
case class JsSwitch(expr: JsExpr, cases: List[JsCase], default: Option[JsDefault]) extends JsStmt
case class JsVarDef(idents: List[(String, JsExpr)]) extends JsStmt
case class JsFunDecl(ident: String, params: List[String], body: JsStmt) extends JsStmt
case class JsObjDecl(name: String, params: List[String], fields: List[(String, JsExpr)]) extends JsStmt
case class JsReturn(jsExpr: JsExpr) extends JsStmt
case class JsStmts(stmts: List[JsStmt]) extends JsStmt