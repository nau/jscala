package org.jscala

object JavascriptPrinter {
  def print(ast: JsAst, indent: Int): String = {
    def p(ast: JsAst) = print(ast, indent)
    def p2(ast: JsAst) = " " * (indent + 2) + print(ast, indent + 2)
    def s(ast: JsAst) = ast match {
      case _: JsLit => p(ast)
      case _: JsIdent => p(ast)
      case _: JsCall => p(ast)
      case s => s"(${p(ast)})"
    }
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
      case JsSelect(qual, "apply")              => s"${p(qual)}"
      case JsSelect(qual, name)                 => s"${p(qual)}.$name"
      case JsUnOp(operator, operand)            => s"${operator}${s(operand)}"
      case JsBinOp("=", lhs@JsIdent(_), rhs)    => s"${p(lhs)} = ${s(rhs)}"
      case JsBinOp("=", lhs@JsSelect(_, _), rhs)=> s"${p(lhs)} = ${s(rhs)}"
      case JsBinOp("=", lhs, rhs)               => s"${s(lhs)} = ${s(rhs)}"
      case JsBinOp(operator, lhs, rhs)          => s"(${s(lhs)}) $operator ${s(rhs)}"
      case JsNew(call)                          => s"""new ${p(call)}"""
      case JsCall(callee, params)               => s"""${p(callee)}(${params.map(p(_)).mkString(", ")})"""
      case JsBlock(stmts)                       => s"""{\n${stmts.map(p2(_)).mkString(";\n")}\n${" " * indent}}"""
      case JsExprStmt(jsExpr)                   => p(jsExpr)
      case JsIf(cond, thenp, elsep)             => s"if (${p(cond)}) ${p(thenp)}" + elsep.map(e => s" else ${p(e)}").getOrElse("")
      case JsSwitch(expr, cases, default)       =>  s"""switch (${p(expr)}) {
                                                    |${cases.map(p2).mkString("\n")}
                                                    |${default.map(p2).getOrElse("")}
                                                    |${" " * indent}}""".stripMargin
      case JsCase(const, body)                  => s"case ${p(const)}:\n${p2(body)}\n${" " * (indent + 2)}break;"
      case JsDefault(body)                      => s"default:\n${p2(body)}\n${" " * (indent + 2)}break;"
      case JsWhile(cond, body)                  => s"while (${p(cond)}) ${p(body)}"
      case JsTry(body, cat, fin)                => s"""try {
                                                   |  ${p(body)}
                                                   |}${cat.map(p2).getOrElse("")}
                                                   |  ${fin.map(f => s"finally {${p2(f)}\n}").getOrElse("")}""".stripMargin
      case JsCatch(JsIdent(ident), body)        => s"catch($ident) {\n${p2(body)}\n}"
      case JsFor(ident, from, until, body)      => s"for (var ${p(ident)} = ${p(from)}; ${p(ident)} < ${s(until)}; ${p(ident)}++) ${p(body)}"
      case JsForIn(coll, ident, body)           => s"for (${p(ident)} in ${p(coll)}) ${p(body)}"
      case JsVarDef(ident, JsUnit)              => s"var ${ident}"
      case JsVarDef(ident, initializer)         => s"var ${ident} = ${p(initializer)}"
      case JsFunDecl(ident, params, body)       => s"""function ${ident}(${params.mkString(", ")}) ${p(body)}"""
      case JsAnonFunDecl(params, body)          => s"""(function (${params.mkString(", ")}) ${p(body)})""" // Wrap in parens to allow easy IIFEs
      case JsAnonObjDecl(fields)                => fields.map{ case (k, v) => s""" $k: ${p(v)}"""}.mkString("{", ",\n", "}")
      case JsReturn(jsExpr)                     => s"return ${p(jsExpr)};"
      case JsUnit                               => ""
    }
  }
}
