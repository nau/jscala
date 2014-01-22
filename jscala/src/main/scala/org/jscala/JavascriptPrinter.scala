package org.jscala

object JavascriptPrinter {
  private[this] val substitutions = Map("\\\"".r -> "\\\\\"", "\\n".r -> "\\\\n", "\\r".r -> "\\\\r", "\\t".r -> "\\\\t")
  private[this] def simplify(ast: JsAst): JsAst = ast match {
    case JsBlock(stmts) => JsBlock(stmts.filter(_ != JsUnit))
    case JsCase(const, JsBlock(List(stmt))) => JsCase(const, stmt)
    case JsDefault(JsBlock(List(stmt))) => JsDefault(stmt)
    case t => t
  }

  def print(ast: JsAst, indent: Int): String = {
    def ind(c: Int = 0) = " " * (indent + c)
    def p(ast: JsAst) = print(ast, indent)
    def p2(ast: JsAst) = ind(2) + p3(ast)
    def p3(ast: JsAst) = print(ast, indent + 2)
    def p4(ast: JsAst) = ind() + p(ast)
    def s(ast: JsAst) = ast match {
      case _: JsLit => p(ast)
      case _: JsIdent => p(ast)
      case _: JsCall => p(ast)
      case s => s"(${p(ast)})"
    }
    def !< = "{\n"
    def !> = "\n" + ind() + "}"
    simplify(ast) match {
      case JsLazy(f)                            => p(f())
      case JsNull                               => "null"
      case JsBool(value)                        => value.toString
      case JsString(value)                      => "\"" + substitutions.foldLeft(value){case (v, (r, s)) => r.replaceAllIn(v, s)} + "\""
      case JsNum(value, true)                   => value.toString
      case JsNum(value, false)                  => value.toLong.toString
      case JsArray(values)                      => values.map(p).mkString("[", ", ", "]")
      case JsIdent(value)                       => value
      case JsRaw(value)                         => value
      case JsAccess(qual, key)                  => s"${p(qual)}[${p(key)}]"
      case JsSelect(qual, "apply")              => p(qual)
      case JsSelect(qual, name)                 => s"${p(qual)}.$name"
      case JsUnOp(operator, operand)            => operator + s(operand)
      case JsBinOp("=", lhs, rhs)               => s"${p(lhs)} = ${p(rhs)}"
      case JsBinOp(operator, lhs: JsBinOp, rhs: JsBinOp) => s"${s(lhs)} $operator ${s(rhs)}"
      case JsBinOp(operator, lhs: JsBinOp, rhs) => s"${s(lhs)} $operator ${p(rhs)}"
      case JsBinOp(operator, lhs, rhs: JsTernary) => s"${s(lhs)} $operator ${s(rhs)}"
      case JsBinOp(operator, lhs, rhs: JsBinOp) => s"${p(lhs)} $operator ${s(rhs)}"
      case JsBinOp(operator, lhs, rhs)          => s"${p(lhs)} $operator ${p(rhs)}"
      case JsNew(call)                          => s"new ${p(call)}"
      case expr@JsCall(JsSelect(callee: JsLazy[_], "apply"), params) => s"""(${p(callee)})(${params.map(p(_)).mkString(", ")})"""
      case JsCall(JsSelect(callee: JsAnonFunDecl, "apply"), params) => s"""(${p(callee)})(${params.map(p(_)).mkString(", ")})"""
      case JsCall(callee, params)               => s"""${p(callee)}(${params.map(p(_)).mkString(", ")})"""
      case JsBlock(Nil)                         => "{}"
      case JsBlock(stmts)                       => !< + stmts.map(p2(_) + ";\n").mkString + ind() + "}"
      case JsTernary(cond, thenp, elsep)        => s"${s(cond)} ? ${p(thenp)} : ${p(elsep)}"
      case JsIf(cond, expr: JsExpr, Some(els: JsExpr)) => s"${s(cond)} ? ${p(expr)} : ${p(els)}"
      case JsIf(cond, thenp, elsep)             => s"if (${p(cond)}) ${p(thenp)}" + elsep.map(e => s" else ${p(e)}").getOrElse("")
      case JsSwitch(expr, cases, default)       =>  s"switch (${p(expr)}) " +
        !< + cases.map(p2).mkString("\n") + default.map(d => "\n" + p2(d)).getOrElse("") + !>
      case JsCase(consts, body)                 => consts.map(c => s"case ${p(c)}:\n").mkString(ind()) + p2(body) + ";\n" + ind(2) + "break;"
      case JsDefault(body)                      => "default:\n" + p2(body) + ";\n" + ind(2) + "break;"
      case JsWhile(cond, body)                  => s"while (${p(cond)}) ${p(body)}"
      case JsTry(body, cat, fin)                =>
        val (b, c, f) = (p(body), cat.map(p2).getOrElse(""), fin.map(f => s"finally {${p2(f)}\n}").getOrElse(""))
                                                   s"try { $b \n} $c \n $f"
      case JsCatch(JsIdent(ident), body)        => s"catch($ident) {\n${p2(body)}\n}"
      case JsFor(init, check, update, body)     =>
        val in = init.map(p).mkString(", ")
        val upd = update.map(p).mkString(", ")
        s"for ($in; ${p(check)}; $upd) ${p(body)}"
      case JsForIn(JsIdent(ident), coll, body)  => s"for (var $ident in ${p(coll)}) ${p(body)}"
      case JsVarDef(Nil)                        => sys.error("Var definition must have at least one identifier.")
      case JsVarDef(idents)                     => "var " + idents.map {
                                                      case (ident, JsUnit) => ident
                                                      case (ident, init) => ident + " = " + p(init)
                                                    }.mkString(", ")
      case JsFunDecl(ident, params, body)       => s"""function $ident(${params.mkString(", ")}) ${p(body)}"""
      case JsAnonFunDecl(params, body)          => s"""function (${params.mkString(", ")}) ${p3(body)}"""
      case JsAnonObjDecl(fields)                =>
        if (fields.isEmpty) "{}" else fields.map{ case (k, v) => ind(2) + s""""$k": ${p(v)}"""}.mkString(!<, ",\n", !>)
      case JsObjDecl(name, JsFunDecl(_, params, JsBlock(stmts)), fields) =>
        val fs = for ((n, v) <- fields) yield ind(2) + s"this.$n = ${p(v)};"
        val body = fs ++ stmts.map(s => ind(2) + p(s)) mkString "\n"
        s"""function $name(${params.mkString(", ")}) {\n$body\n${ind()}}"""
      case JsReturn(jsExpr)                     => s"return ${p(jsExpr)}"
      case JsUnit                               => ""
      case JsStmts(stmts)                       => p(stmts.head) + ";\n" + stmts.tail.map(p4(_)).mkString(";\n")
    }
  }
}
