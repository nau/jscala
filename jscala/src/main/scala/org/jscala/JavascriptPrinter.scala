package org.jscala

object JavascriptPrinter {
  def print(ast: JsAst, indent: Int): String = {
    def p(ast: JsAst) = print(ast, indent)
    def ind(c: Int = 0) = " " * (indent + c)
    def p2(ast: JsAst) = ind(2) + print(ast, indent + 2)
    def p3(ast: JsAst) = print(ast, indent + 2)
    def s(ast: JsAst) = ast match {
      case _: JsLit => p(ast)
      case _: JsIdent => p(ast)
      case _: JsCall => p(ast)
      case s => s"(${p(ast)})"
    }
    def !< = "{\n"
    def !> = "\n" + ind() + "}"
    ast match {
      case JsLazy(f)                            => p(f())
      case JsBool(value)                        => value.toString
      case JsString(value)                      => "\"" + value + "\""
      case JsNum(value, true)                   => value.toString
      case JsNum(value, false)                  => value.toLong.toString
      case JsArray(values)                      => values.map(p).mkString("[", ", ", "]")
      case JsIdent(value)                       => value
      case JsRaw(value)                         => value
      case JsAccess(qual, key)                  => s"${p(qual)}[${p(key)}]"
      case JsSelect(qual, "apply")              => p(qual)
      case JsSelect(qual, name)                 => s"${p(qual)}.$name"
      case JsUnOp(operator, operand)            => operator + s(operand)
      case JsBinOp(operator, lhs, rhs)          => s"${p(lhs)} $operator ${p(rhs)}"
      case JsNew(call)                          => s"""new ${p(call)}"""
      case JsCall(callee, params)               => s"""${p(callee)}(${params.map(p(_)).mkString(", ")})"""
      case JsBlock(stmts)                       => !< + stmts.map(p2(_) + ";\n").mkString + ind() + "}"
      case JsExprStmt(jsExpr)                   => p(jsExpr)
      case JsIf(cond, thenp, elsep)             => s"if (${p(cond)}) ${p(thenp)}" + elsep.map(e => s" else ${p(e)}").getOrElse("")
      case JsSwitch(expr, cases, default)       =>  s"""switch (${p(expr)}) {
                                                    |${cases.map(p2).mkString("\n")}
                                                    |${default.map(p2).getOrElse("")}
                                                    |${" " * indent}}""".stripMargin

      case JsCase(const, body)                  => s"case ${p(const)}:\n${p2(body)}\n${ind(2)}break;"
      case JsDefault(body)                      => s"default:\n${p2(body)}\n${ind(2)}break;"
      case JsWhile(cond, body)                  => s"while (${p(cond)}) ${p(body)}"
      case JsTry(body, cat, fin)                =>
        val (b, c, f) = (p(body), cat.map(p2).getOrElse(""), fin.map(f => s"finally {${p2(f)}\n}").getOrElse(""))
                                                   s"try { $b \n} $c \n $f"

      case JsCatch(JsIdent(ident), body)        => s"catch($ident) {\n${p2(body)}\n}"
      case JsFor(ident, from, until, body)      => s"for (var ${p(ident)} = ${p(from)}; ${p(ident)} < ${p(until)}; ${p(ident)}++) ${p(body)}"
      case JsForIn(coll, ident, body)           => s"for (${p(ident)} in ${p(coll)}) ${p(body)}"
      case JsVarDef(ident, JsUnit)              => s"var $ident"
      case JsVarDef(ident, initializer)         => s"var $ident = ${p(initializer)}"
      case JsFunDecl(ident, params, body)       => s"""function $ident(${params.mkString(", ")}) ${p(body)}"""
      case JsAnonFunDecl(params, body)          => s"""(function (${params.mkString(", ")}) ${p3(body)})""" // Wrap in parens to allow easy IIFEs
      case JsAnonObjDecl(fields)                =>
        if (fields.isEmpty) "{}" else fields.map{ case (k, v) => ind(2) + s"""$k: ${p(v)}"""}.mkString(!<, ",\n", !>)
      case JsObjDecl(name, params, fields)              =>
        val fs = (for ((n, v) <- fields) yield ind(2) + s"this.$n = ${p(v)}").mkString(";\n")
        val body = fs
        s"""function $name(${params.mkString(", ")}) {\n$body\n${ind()}}"""
      case JsReturn(jsExpr)                     => s"return ${p(jsExpr)}"
      case JsUnit                               => ""
    }
  }
}
