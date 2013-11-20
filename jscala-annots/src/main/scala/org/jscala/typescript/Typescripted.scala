/* Typescript to scala macro annotation
 * Copyright 2013 Apyx
 * @author  Arnaud PEZEL
 */
package org.jscala.typescript

import scala.annotation.StaticAnnotation

import language.experimental.macros
import language.implicitConversions
import scala.reflect.macros.Context
import com.apyx.scala.ts2scala.macros._

object TypescriptedMacro {
  def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    TS2Scala._impl(JScalaDialect)(c)(annottees:_*)
  }
}

class Typescripted(file:String) extends TS2Scala(file) {
  override def macroTransform(annottees: Any*) = macro TypescriptedMacro.impl
}