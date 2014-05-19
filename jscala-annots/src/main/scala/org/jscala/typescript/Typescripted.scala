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
    val gen = TS2Scala._impl(JScalaDialect)(c)(annottees:_*)
//    println(gen)
    gen
  }
}

class Typescripted(file:String) extends StaticAnnotation {
  def macroTransform(annottees: Any*) = macro TypescriptedMacro.impl
}